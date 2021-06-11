library(patchwork)
library(moderndive)
library(DALEX)

#' Inspect missing values
#'
#' @param data `data.frame` to inspect
#' @param main_variable column to use for grouping, usually product column
#' @param title optional, title to display
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' inspect_missing_values(data, product, "Missing value check")
#' }
inspect_missing_values <- function(data, main_variable, title = NULL) {
  data %>%
    mutate(across(-{{main_variable}}, is.na)) %>%
    pivot_longer(-{{main_variable}}) %>%
    group_by({{main_variable}}, name) %>%
    summarise(missing_pct = mean(value)) %>%
    ggplot(aes(name, product, fill = missing_pct)) +
    geom_tile() +
    scale_fill_distiller(palette = "Blues", direction = 1, labels = scales::percent) +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
}

semi_join_silent <- function(...) {
  suppressMessages(semi_join(...))
}

makeCartesianResampling <- function(data, ...) {
  dts <- enquos(...)
  dims <- names(dts)
  if(!all(dims %in% names(data))) stop('Some sampling IDs are missing from the data set.')
  
  data_rownum <- data %>%
    dplyr::select(all_of(dims)) %>%
    mutate(.rownum = row_number())
  
  presplits <- dts %>%
    # presplits <- dts %>%
    imap(~{
      rsmpl <- lazyeval::f_rhs(.x)
      rsmpl <- possibly(eval, rsmpl)(rsmpl)
      if(is_formula(rsmpl)) rsmpl <- lazyeval::f_rhs(rsmpl)
      
      name <- .y
      
      ids <- data %>%
        distinct(!!sym(name))
      
      rsmpl$data <- quote(ids)
      eval(rsmpl) %>%
        # lazyeval::f_eval(rsmpl) %>%
        rename_all(sprintf, fmt = '%s_%s', name) %>%
        mutate(foo = 1)
    }) %>%
    reduce(inner_join, by = 'foo') %>%
    dplyr::select(-foo) %>%
    split(seq(nrow(.))) %>%
    map(~{
      splits <- .x %>% 
        dplyr::select(starts_with('splits_')) %>%
        dplyr::rename_with(gsub, pattern = '^splits_', replacement = '')
      id <- .x %>% 
        dplyr::select(starts_with('id_')) %>%
        dplyr::rename_with(gsub, pattern = '^id_', replacement = '') %>%
        imap(sprintf, fmt = '%2$s_%1$s') %>% 
        reduce(sprintf, fmt = '%s-%s')
      
      assesment_rows <- splits %>%
        map(~.x[[1]]) %>%
        map(assessment) %>%
        reduce(semi_join_silent, .init = data_rownum) %>%
        pull(.rownum) %>%
        as.integer()
      
      
      analysis_rows <- splits %>%
        map(~.x[[1]]) %>%
        map(analysis) %>%
        reduce(semi_join_silent, .init = data_rownum) %>%
        pull(.rownum) %>%
        as.integer()
      
      list(rows = list(assessment = assesment_rows, analysis = analysis_rows),
           id = id)
      
    }) %>%
    transpose()
  
  manual_rset(map(presplits$rows, make_splits, data = data),
              unlist(presplits$id))
  
}

applyResamplingStrategy <- function(data, strategy) {
  strategy %>%
    imap(~{
      tmp <- lazyeval::f_rhs(.x)
      tmp$data <- data
      eval(tmp)
    }) %>%
    prepend(list(train = data))
}

makeLOOID <- function(resampling, id_name) {
  id_name <- enquo(id_name)
  
  resampling %>%
    mutate(id = map_chr(splits,
                        ~{
                          .x %>%
                            assessment() %>%
                            distinct(!! id_name) %>%
                            pull(!! id_name)
                        }))
}

dev_from_opt <- function(x, opt_val) {
  rmse_vec(rep(opt_val, length(x)), x)
}

makeMeansPlot <- function(data, label, ..., opt_val = 3) {
  facet_vars <- enquos(...)
  
  n_facet <- data %>%
    ungroup() %>%
    distinct(!!! facet_vars) %>% 
    nrow()
  
  if(is.ordered(data$Target)) {
    lvls <- data %>%
      select(matches("\\.pred_[0-9]")) %>%
      colnames() %>%
      gsub(pattern = "\\.pred_", replacement = "") %>%
      sort()
    
    pred <- data %>%
      select(!!! facet_vars, product, matches("\\.pred_[0-9]")) %>%
      group_by(!!! facet_vars, product) %>%
      summarise(across(matches("\\.pred_[0-9]"), mean)) %>%
      pivot_longer(matches("\\.pred_[0-9]"), values_to = ".pred") %>%
      mutate(Target = factor(gsub("\\.pred_", "", name), levels = lvls, ordered = TRUE)) %>%
      select(-name)
    
    tg <- data %>%
      group_by(!!! facet_vars, product) %>%
      count(Target) %>%
      mutate(target = n/sum(n))
    
    pred_tg <-  pred %>%
      full_join(tg) %>%
      mutate(target_num = as.numeric(as.character(Target))) %>%
      select(-Target) %>%
      rename(Target = target) %>%
      mutate(across(c(n, .pred, Target), replace_na, 0)) %>%
      mutate(opt = opt_val) %>%
      group_by(!!! facet_vars, product) %>%
      summarise(across(c(.pred, Target), 
                       ~mltools::rmse(opt, target_num, weights =  .))) 
  } else if(is.numeric(data$Target)) {
    pred_tg <- data %>%
      select(!!! facet_vars, product, .pred, Target) %>%
      group_by(!!! facet_vars, product) %>%
      summarise(across(c(.pred, Target), mean))
  }
  
  data_agg <- pred_tg %>%
    nest(data = - c(!!! facet_vars)) %>%
    mutate(adj_r_sq = data %>%
             map(lm, formula = .pred ~ Target) %>%
             map(get_regression_summaries) %>%
             map_dbl(2) %>%
             sprintf(fmt = "Adj R Sqr = %.3f")) %>%
    mutate(label = paste(!!! facet_vars, adj_r_sq, sep = ', ')) %>%
    unnest(data)
  
  lm_model <- lm(.pred ~ Target, data = data_agg) 
  
  adj_r_sq <- lm_model %>% 
    get_regression_summaries() %>% 
    pluck(2)
  
  p_calibration <- data_agg %>%
    ggplot(aes(x = Target, y = .pred, label = product)) + 
    geom_smooth(method = "lm") + 
    geom_label() + 
    ggtitle(str_c(label, ": Adjusted R Squared = ", adj_r_sq)) +
    facet_wrap(~label, nrow = ceiling(sqrt(n_facet)))
  
  p_calibration
}


fixGLMnetModel <- function(object) {
  if("workflow" %in% class(object)) {
    object$fit$fit$spec$method$pred$numeric$args$newx <- 
      expr(as.matrix(new_data[, rownames(object$fit$beta), drop = FALSE]))
  } else if("model_fit" %in% class(object)) {
    object$spec$method$pred$numeric$args$newx <- 
      expr(as.matrix(new_data[, rownames(object$fit$beta), drop = FALSE]))
  }
  
  object
}

# predict_product <- function(product_model, product_data, consumer_data = NULL) {
#   
#   if(class(product_model) == 'list') model <- product_model$model 
#   if(is.null(consumer_data)) consumer_data <- product_model$consumer_data
#   
#   product_data %>%
#     mutate(rn = row_number()) %>%
#     inner_join(consumer_data, by = character()) %>%
#     bind_cols(predict(object = model, .)) %>%
#     mutate(.pred = pmin(11, .pred * SD + Mean)) %>%
#     group_by(rn) %>%
#     summarise(Target = mean(.pred)) %>%
#     arrange(rn) %>%
#     pull('Target')
# }
# 
# predict_product_consumer <- function(product_model, product_data, 
#                                      consumer_data = NULL, scaling_table = NULL) {
#   
#   if(class(product_model) == 'list') model <- product_model$model 
#   if(is.null(consumer_data)) consumer_data <- product_model$consumer_data
#   if(is.null(scaling_table)) scaling_table <- product_model$scaling_table
#   
#   max_pred <- max(scaling_table$pred_h) - 1e-5
#   min_pred <- min(scaling_table$pred_l)
#   
#   product_data %>%
#     mutate(rn = row_number()) %>%
#     inner_join(consumer_data, by = character()) %>%
#     bind_cols(predict(object = model, .)) %>%
#     mutate(.pred_unscaled = pmin(11, .pred * SD + Mean)) %>%
#     mutate(.pred_calibrated = pmax(min_pred, pmin(max_pred, .pred_unscaled))) %>%
#     inner_join(scaling_table, by = character()) %>%
#     filter(.pred_calibrated >= pred_l & .pred_calibrated < pred_h) %>%
#     mutate(.pred_calibrated = tg - 0.5 + (.pred_calibrated - pred_l)/(pred_h - pred_l)) %>%
#     select(
#       Product,
#       Consumer, 
#       .pred_rescaled = .pred,
#       .pred_unscaled,
#       .pred_calibrated
#     )
# }



createProductModel <- function(model, consumer_data = NULL, model_type = "num", opt_val = NULL) {
  obj <- structure(
    list(model = model,
         consumer_data = consumer_data),
    class = sprintf("product_model_%s", model_type)
  )
  
  if(model_type == "jar")
    obj$opt_val <- opt_val
  
  obj
}

predict.product_model_num <- function(product_model, product_data, consumer_data = NULL, ...) {
  
  model <- product_model$model
  consumer_data <- product_model$consumer_data
  
  product_data %>%
    mutate(rn = row_number()) %>%
    inner_join(consumer_data, by = character()) %>%
    bind_cols(predict(object = model, .)) %>%
    group_by(rn) %>%
    summarise(mean = mean(.pred)) %>%
    arrange(rn) %>%
    pull('mean')
}

predict.product_model_jar <- function(product_model, product_data, consumer_data = NULL, opt_val = NULL, ...) {
  
  model <- product_model$model
  consumer_data <- product_model$consumer_data
  opt_val <- product_model$opt_val
  
  product_data %>%
    mutate(rn = row_number()) %>%
    inner_join(consumer_data, by = character()) %>%
    bind_cols(predict(object = model, ., type = "prob")) %>%
    group_by(rn) %>%
    summarise(across(matches("\\.pred_[0-9]"), mean)) %>%
    pivot_longer(matches("\\.pred_[0-9]"), values_to = ".pred") %>%
    mutate(Target = as.numeric(gsub("\\.pred_", "", name)),
           opt = opt_val) %>%
    group_by(rn) %>%
    summarise(Overall = mltools::rmse(opt, Target, weights = .pred)) %>%
    arrange(rn) %>%
    pull('Overall')
}

calculateVariableImportance <- function(model, product_data, type = "num") {
  pred_type = ifelse(type == "num", "numeric", "prob")
  baseline_preds <- predict(model, product_data, type = pred_type)
  colnames(product_data) %>%
    keep(~{
      tmp <- product_data
      tmp[[.x]] <- sample(tmp[[.x]], nrow(tmp))
      
      any(predict(model, tmp, type = pred_type) != baseline_preds)
    })
}

addLabelColumn <- function(data, x) {
  x <- enquo(x)
  
  data %>%
    mutate(foo = 'ALL',
           foo2 = !! x) %>%
    pivot_longer(c(foo, foo2), values_to = sprintf("%s_label", quo_name(x))) %>%
    select(-name)
}

addLabelColumns <- function(data, ...) {
  x <- enquos(...)
  
  x %>%
    reduce(addLabelColumn, .init = data)
}


processModelResults <- function(fitted, ..., opt_val = 3) {
  x <- enquos(...)
  x_labels <- map(x,
                  ~{
                    new_expr <- .x  %>% 
                      quo_get_expr() %>%
                      paste0('_label') %>%
                      as.name()
                    
                    quo_set_expr(.x, new_expr)
                  })
  
  
  fitted %>%
    mutate(test_pred_df = data %>%
             map(mutate, .row = row_number()) %>%
             map2(cv_predictions, inner_join)) %>%
    # mutate(fwf = fixGLMnetModel(fwf)) %>%
    mutate(data = data %>%
             map(addLabelColumns, !!! x) %>%
             map(group_by, !!! x_labels) %>%
             map(nest)) %>%
    unnest(data) %>%
    # mutate(label = map2(Code, model_name, paste, sep = ' - '),
    #        test_pred_df = map2(test_pred_df, data, semi_join, by = c("product", "consid"))) %>%
    # mutate(means_plot = map2(test_pred_df, label, makeMeansPlot)) %>%
    # select(data, test_pred_df, means_plot)
    mutate(consumer_data = data %>%
             map(select, consid, any_of(consumer_variables)) %>%
             map(distinct),
           product_data = data %>%
             map(select, product, any_of(product_variables), Target) %>%
             map(mutate, Target = as.numeric(as.character(Target))) %>%
             map(group_by, product) %>%
             map2(type, 
                  ~{
                    .x %>%
                      mutate(Target = if_else(.y == "jar",
                                              dev_from_opt(Target, opt_val = opt_val),
                                              mean(Target)))
                    }) %>%
             map(ungroup) %>%
             map(distinct)) %>%
    mutate(model = pmap(list(fwf, consumer_data, type), createProductModel, opt_val = opt_val)) %>%
    select(Code, !!! x_labels, type, product_data, model)
}

findOptimalProduct <- function(data) {
  data %>%
    mutate(optimized =
             pmap(list(model, important_variables, product_data), possibly(run_optimization, NULL))) %>%
    mutate(comparison_mat = map(optimized, "comparison_mat"),
           optimal_prediction = map(optimized, "optimal_prediction"))
}

select_best_custom <- function(x, type) {
  if(type == "num") {
    return(select_best(x))
  } else {
    preds <- collect_predictions(x)
    
    selected <- preds %>%
      count(.config) %>%
      filter(n == max(n))
    
    best_config <- preds %>%
      semi_join(selected) %>%
      pivot_longer(matches("\\.pred_[0-9]")) %>%
      mutate(Target = as.numeric(as.character(Target)),
             .pred = as.numeric(gsub("\\.pred_", "", name))) %>%
      group_by(.config) %>%
      summarise(rmse = mltools::rmse(Target, .pred, value)) %>%
      slice_min(rmse, n = 1) %>%
      slice(1) %>%
      pull(.config)
    
    preds %>%
      filter(.config == best_config) %>%
      distinct(mixture, penalty, .config)
  }
}