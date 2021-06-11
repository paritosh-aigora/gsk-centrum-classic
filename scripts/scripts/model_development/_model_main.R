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
source_stem <- "gummy"
# source_stem <- "gummy_color"

output_path <- file.path("output", "model", source_stem)
if(!dir.exists(output_path)) {
  dir.create(output_path)
  dir.create(file.path(output_path, "plots"))
  dir.create(file.path(output_path, "model_studio"))
}
source(file.path('scripts', 'model_development', sprintf('data_for_model_%s.R', source_stem)))

# Define models -----------------------------------------------------------

source(file.path('scripts', 'model_development', 'model_config.r'))

# Fit model ---------------------------------------------------------------

# plan(multicore, workers = availableCores() - 1)

to_fit <- all_data %>%
  inner_join(model_configs, by = "type") %>%
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
                map2(type, select_best_custom) %>%
                # map(select_best, metric = selection_metric) %>%
                map2(wf, ~finalize_workflow(.y, .x)) %>%
                map2(data, fit)) %>%
       mutate(cv_predictions = fwf %>%
                map2(rsmpl, fit_resamples,
                     control = control_resamples(save_pred = TRUE)) %>%
                map(collect_predictions)) %>%
       mutate(important_variables =
                future_pmap(list(fwf, data, type), calculateVariableImportance,
                            .options = furrr_options(seed = 1))) %>%
       mutate(data = map2(data, important_variables,
                          ~select(.x, any_of(c("consid", "product", "Target", 
                                               .y))))) %>%
       mutate(rcp = map2(data, pre_rcp, ~.y(.x))) %>%
       mutate(wf = model %>%
                map(add_model, x = workflow()) %>%
                map2(rcp, add_recipe)) %>%
       mutate(fwf = grid_results %>%
                map2(type, select_best_custom) %>%
                map2(wf, ~finalize_workflow(.y, .x)) %>%
                map2(data, fit)) %>%
       mutate(cv_predictions = fwf %>%
                map2(rsmpl, fit_resamples,
                     control = control_resamples(save_pred = TRUE)) %>%
                map(collect_predictions))
)

# Analyse results ---------------------------------------------------------

write_rds(fitted, file.path(output_path,  str_c(Sys.Date(), " - fitted.rds")))

# fitted <- read_rds(file.path("output", "model",  "2021-03-30 - fitted.rds"))

results <- fitted %>%
  mutate(test_pred_df = data %>%
           map(mutate, .row = row_number()) %>%
           map2(cv_predictions, inner_join)) %>%
  mutate(label = paste(Code, model_name, sep = ' - ')) %>%
  mutate(means_plot_all = map2(test_pred_df, label,
                               makeMeansPlot),
         grid_plot = map(grid_results, autoplot))

plot_file <- file.path(output_path, "plots", str_c(Sys.Date(), " - model_means.pdf"))
graphics.off()
pdf(plot_file, width = 8, height = 8)
results %>%
  pivot_longer(starts_with("means_plot")) %>%
  pluck("value") %>%
  walk(print)
dev.off()

final_models <- fitted %>%
  mutate(Code = sprintf("%s-%s", Code, model_name)) %>%
  select(-model_name) %>%
  processModelResults() %>%
  mutate(important_variables =
           pmap(list(model, product_data, type), calculateVariableImportance))

write_rds(final_models, file.path(output_path,  str_c(Sys.Date(), " - final_models.rds")))

optimized <- final_models %>%
  findOptimalProduct

write_rds(optimized, file.path(output_path,  str_c(Sys.Date(), " - optimized.rds")))

# Model Studio ------------------------------------------------------------

optimized <- read_rds(file.path(output_path,  "2021-05-14 - optimized.rds"))

optimized2 <- optimized %>%
  filter(!map_lgl(optimal_prediction, is.null)) %>%
  mutate(product_data = optimal_prediction %>%
           map(mutate, product = "Optimized") %>%
           map2(product_data, bind_rows))

# models <- final_models
models <- optimized2 %>% slice(-(1:4))
source(file.path('scripts', 'model_development', 'model_studio.R'))


