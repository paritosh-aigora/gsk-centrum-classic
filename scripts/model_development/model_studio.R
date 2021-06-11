# library(tidyverse)
# library(tidymodels)
# library(rsample)
# library(DALEX)
# 
# # remotes::install_github("kwiscion/kwiscion", ref = "main")
# library(kwiscion)
# 
# source(file.path('scripts', 'model_development', 'model_helpers.R'))
# # source(file.path('scripts', 'model_development', 'run_optimization.R'))
# # source(file.path('scripts', 'supporting', 'load_functions_model_kuba.R'))

library(modelStudio)
# library(parallelMap)
# options(
#   parallelMap.default.mode        = "socket",
#   parallelMap.default.cpus        = 6,
#   parallelMap.default.show.info   = FALSE
# )
# 
# optimized2 <- optimized %>%
#   filter(!map_lgl(optimal_prediction, is.null)) %>%
#   mutate(product_data = optimal_prediction %>%
#            map(mutate, product = "Optimized") %>%
#            map2(product_data, bind_rows))
# 
# # models <- final_models
# models <- optimized2

for(mdl in transpose(models)) {
  expl <- explain(
    model = mdl$model,
    data = mdl$product_data %>%
      select(-product, -Target),
    y = mdl$product_data$Target,
    predict_function = mdl$predict
  )
  
  ms <- modelStudio(
    explainer = expl,
    new_observation = mdl$product_data %>%
      mutate(rowname = product) %>%
      column_to_rownames() %>%
      select(-product, -Target) ,
    new_observation_y = round(mdl$product_data$Target, 2),
    N = 50,
    B = 5,
    parallel = FALSE
  )
  
  r2d3::save_d3_html(ms, 
                     here::here(file.path(output_path, 'model_studio', 
                                          sprintf('%s.html', mdl$Code))))
}


# system("git add .")
# system('git commit -m "modelStudio"')
# system('git pull')
# system('git push')
