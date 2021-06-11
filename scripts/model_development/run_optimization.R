
library(FactoMineR)
library(factoextra)
library(dendextend)
library(mice)

run_optimization <- function(model, important_vars, product_data) {
  # prepare for optimization
  # print(optimal_value)
  min_vals <- product_data %>%
    select(-product, -Target) %>% 
    map_dbl(min, na.rm = TRUE)
  
  max_vals <- product_data %>% 
    select(-product, -Target) %>% 
    map_dbl(max, na.rm = TRUE)
  
  med_vals <- product_data %>% 
    select(-product, -Target) %>% 
    map_dbl(median, na.rm = TRUE)
  
  # create function for product vector as function of components
  prod_res <- predict(model, product_data)
  best_ind <- which(prod_res == min(prod_res))
  
  best_prod_profile <- product_data[best_ind,] %>%
    select(-Target)
  
  input_vect <- best_prod_profile %>%
    select(one_of(important_vars)) %>%
    as.numeric()
  
  base_data <- med_vals %>% 
    bind_rows()
  
  predict_liking_full <- function(input_vect, model, important_vars, base_data){
    
    base_data[1, important_vars] <- input_vect %>% 
      set_names(important_vars) %>% 
      bind_rows()
    
    suppressWarnings(output <- predict(model, base_data))
    
    if(class(model) == "product_model_num")
      output <- -output
    
    return(output)
  }
  
  predict_liking <- partial(predict_liking_full, 
                            model = model, 
                            important_vars = important_vars, 
                            base_data = base_data)
  
  # predict_liking(input_vect)
  
  # optimize
  
  min_mat <- min_vals[important_vars] %>% 
    set_names(important_vars) %>% 
    enframe(value = "Min")
  
  max_mat <- max_vals[important_vars] %>% 
    set_names(important_vars) %>% 
    enframe(value = "Max")
  
  max_range <- c(0, 100)
  
  range_mat <- min_mat %>% 
    left_join(max_mat) %>% 
    mutate(range = map2(Min, Max, function(x,y) extendrange(c(x,y))), 
           Min_e = pmax(min(max_range), map_dbl(range, pluck(1))), 
           Max_e = pmin(max(max_range), map_dbl(range, pluck(2))))
  
  start_vect <- base_data %>% 
    select(all_of(important_vars)) %>% 
    as.numeric()
  
  opt_res <- optim(start_vect, fn = predict_liking, 
                   lower = range_mat$Min_e, upper = range_mat$Max_e, 
                   method = "L-BFGS-B", control = list(trace = 6))
  
  final_res <- opt_res$par
  
  opt_mat <- final_res %>% 
    set_names(important_vars) %>% 
    enframe(value = "Opt")
  
  best_mat <- input_vect %>% 
    set_names(important_vars) %>% 
    enframe(value = "Best")
  
  comparison_mat <- opt_mat %>% 
    left_join(best_mat) %>% 
    left_join(min_mat) %>% 
    left_join(max_mat) %>% 
    mutate(Diff = Opt - Best) %>% 
    arrange(desc(abs(Diff)))
  
  # impute missing values to fill out profile
  
  opt_row <- rep(NA, ncol(product_data)) %>% 
    set_names(names(product_data)) %>% 
    bind_rows() %>% 
    mutate(product = "Opt")
  
  opt_row[important_vars] <- final_res %>% 
    set_names(important_vars) %>% 
    bind_rows() 
  
  pre_impute_data <- product_data %>% 
    bind_rows(opt_row) %>% 
    select(-Target)
  
  impute_vars <- pre_impute_data %>% 
    select(-product, -one_of(important_vars)) %>% 
    names()
  
  # set.seed(32431)
  # mice_res <- mice(pre_impute_data)
  
  imp_res <- pre_impute_data %>%
    select(-product) %>%
    missMDA::imputePCA() %>%
    pluck("completeObs") %>%
    tail(1) %>%
    as.data.frame() %>%
    select(one_of(impute_vars))
  
  print(imp_res)
  opt_row[impute_vars] <- imp_res[impute_vars]
  
  print(opt_row)
  
  list(
    optimal_prediction = opt_row,
    comparison_mat = comparison_mat)
}
