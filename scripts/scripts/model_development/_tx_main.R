library(tidyverse)
library(tidymodels)
library(rsample)
library(DALEX)
library(furrr)
# remotes::install_github("aigorahub/aigoraOpen")
library(aigoraOpen)
library(earth)

source(file.path('scripts', 'model_development', 'model_helpers.R'))
source(file.path('scripts', 'model_development', 'run_optimization.R'))

# Preprocess data ---------------------------------------------------------

# source_stem <- "tablet"
# source_stem <- "tablet(18-35)"
# source_stem <- "tablet(36-66)"
# source_stem <- "gummy"
# source_stem <- "gummy_color"
source_stem <- "gummy_texture"

output_path <- file.path("output", "model", source_stem)
if(!dir.exists(output_path)) {
  dir.create(output_path)
  dir.create(file.path(output_path, "plots"))
  dir.create(file.path(output_path, "model_studio"))
}

modeling_info <- readxl::read_excel("input/match_files/texture+analytical_attributes.xlsx") %>%
  mutate(variable = janitor::make_clean_names(Product_Name))

predictors <- modeling_info %>%
  filter(Type == "Analytical") %>%
  pull(variable)
targets <- modeling_info %>%
  filter(Type == "Texture") %>%
  pull(variable)

imported_data_path <- file.path(
  "input",
  "imported_data",
  "gummy_imported_data.rds")

sensory_list <- readRDS(imported_data_path) %>% 
  deframe()

product_keys <- readxl::read_excel(file.path("input", "data_keys", "gummy_data_key.xlsx"),
                                   "Sample Info") %>%
  select(product = Consumer, sample_name = Panel)


all_data <- sensory_list$sensory %>%
  rename_with(janitor::make_clean_names) %>%
  inner_join(product_keys) %>%
  select(
    product,
    all_of(predictors),
    all_of(targets)
  ) %>% 
  group_by(product) %>% 
  summarize(across(everything(), .fns = mean, na.rm = TRUE)) %>%
  rename_with(sprintf, fmt = "t_num_%s", all_of(targets)) %>%
  rename_with(sprintf, fmt = "an_%s", all_of(predictors)) %>%
  pivot_longer(starts_with("t_num_"), names_to = "Code", values_to = "Target")  %>%
  nest(data = -Code) %>%
  mutate(type = "num") %>%
  mutate(rsmpl = map(data, vfold_cv, v = 9))

source(file.path('scripts', 'model_development', 'model_config_tx.r'))


to_fit <- all_data %>%
  inner_join(model_configs_tx, by = "type") %>%
  mutate(rcp = map2(data, pre_rcp, ~.y(.x))) %>%
  mutate(wf = model %>%
           map(add_model, x = workflow()) %>%
           map2(rcp, add_recipe))


with(set.seed(1),
     fitted <- to_fit %>%
       mutate(grid_results = pmap(list(wf, grid, rsmpl),
                                  ~tune_grid(..1, grid = ..2, resamples = ..3,
                                             control = control_grid(verbose = TRUE,
                                                                    save_pred = TRUE)))) %>%
       mutate(fwf = grid_results %>%
                map(select_best, metric = "rmse") %>%
                map2(wf, ~finalize_workflow(.y, .x)) %>%
                map2(data, fit)) %>%
       mutate(cv_predictions = fwf %>%
                map2(rsmpl, fit_resamples,
                     control = control_resamples(save_pred = TRUE)) %>%
                map(collect_predictions) %>%
                map2(data, ~{
                  .x %>%
                    mutate(product = .y$product[.row],
                           .pred = pmax(0, .pred))
                })) %>%
       mutate(adj_r2 = cv_predictions %>%
                map(lm, formula = .pred ~ Target) %>%
                map(get_regression_summaries) %>%
                map_dbl(2)) %>%
       mutate(label = str_c(Code, ": Adjusted R Squared = ", adj_r2)) %>%
       mutate(plot = map2(cv_predictions, label, ~{
         .x %>%
           ggplot(aes(x = Target, y = .pred, label = product)) + 
           geom_smooth(method = "lm") + 
           geom_label() + 
           ggtitle(.y)
       }))
)

plot_file <- file.path(output_path, "plots", str_c(Sys.Date(), " - model_means.pdf"))
graphics.off()
pdf(plot_file, width = 8, height = 8)
fitted %>%
  pluck("plot") %>%
  walk(print)
dev.off()

write_rds(fitted, file.path(output_path,  str_c(Sys.Date(), " - fitted.rds")))

library(modelStudio)

for(mdl in transpose(fitted)) {
  expl <- explain(
    model = mdl$fwf,
    data = mdl$data %>%
      select(-product, -Target),
    y = mdl$data$Target,
    predict_function = function(model, data) pmax(0, predict(model, data)$.pred)
  )
  
  ms <- modelStudio(
    explainer = expl,
    new_observation = mdl$data %>%
      mutate(rowname = product) %>%
      column_to_rownames() %>%
      select(-product, -Target) ,
    new_observation_y = round(mdl$data$Target, 2),
    N = 50,
    B = 5,
    parallel = FALSE
  )
  
  r2d3::save_d3_html(ms, 
                     here::here(file.path(output_path, 'model_studio', 
                                          sprintf('%s.html', mdl$Code))))
}


