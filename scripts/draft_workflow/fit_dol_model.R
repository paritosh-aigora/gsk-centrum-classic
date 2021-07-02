
# fit dol models ----------------------------------------------------------

# set training proportion
train_prop <- 0.8

# initialze h2o, and start it if not already running
tryCatch(
  expr = {
    h2o.init(startH2O = FALSE, nthreads = -1)
  },
  error = function(e) {
    h2o.init(nthreads = -1)
  },
  finally = {
    
  }
)

print(str_c("Fitting: ", source_stem, " ------------------------------"))

output_dir <- file.path("output", "modeling_results", source_stem)

if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}

load(file.path(
  "input",
  "cleaned_data",
  str_c(source_stem, "_cleaned_data.Rdata")
))

# read in data
xxx <- cleaned_data$x 
ddd <- cleaned_data$d %>%
  mutate_if(is.ordered, factor, ordered = FALSE)
yyy <- cleaned_data$y

# # inspect demographic data
# 
# ddd %>% glimpse()
# 
# temp_d_key <- cleaned_keys$d %>% 
#   filter(str_detect(var, "^D")) %>% 
#   separate(var_name, into = c("var_stem", "var_level"), remove = FALSE, sep = ": ") %>% 
#   arrange(var_stem, var_level) %>% 
#   group_by(var_stem) %>% 
#   mutate(rank = rank(var_level), max_rank = max(rank)) %>% 
#   filter(rank < max_rank)
# 
# ddd <- ddd %>% 
#   select(ID_Y, one_of(temp_d_key$var))

# Split data into Training and Test ----

# y and d are stratified by ID_Y
training_set_y <- get_train_y(train_prop = train_prop)
training_set_d <- get_train_d(train_prop = train_prop)

test_set_y <- get_test_y(train_prop = train_prop)
test_set_d <- get_test_d(train_prop = train_prop)

# x is a simple random sample

training_set_x <- get_train_x(train_prop = train_prop)
test_set_x <- get_test_x(train_prop = train_prop)

# create training dataset

training_set <- training_set_y %>%
  left_join(training_set_d) %>%
  left_join(training_set_x %>%
              group_by(Stim) %>%
              summarize_at(vars(starts_with("X")), mean, na.rm = TRUE)) %>%
  select(one_of(target_variable),
         #Cluster,
         starts_with("D"),
         starts_with("X"))

# define model features

model_features <-
  setdiff(names(training_set), c("ID_Y", target_variable))

num_pred_vars <- model_features %>%
  length()

# clear h2o memory and load training data ---

h2o.removeAll()

training_frame <- training_set  %>%
  select(one_of(target_variable), one_of(model_features)) %>%
  as.h2o(destination_frame = "train")

# train random forests -----

training_seed <-  1234

# Conduct hyperparameter search

grid_space <- list()
grid_space$ntrees <- c(50, 75, 100)
#grid_space$ntrees <- c(37, 50, 75, 100)
grid_space$max_depth <- c(5, 10, 15)
#grid_space$mtries <- round(num_pred_vars * c(0.25, 0.375, 0.5, 0.75))
grid_space$mtries <- round(num_pred_vars * c(0.25, 0.5, 0.75))

rf_grid <-
  h2o.grid(
    y = target_variable,
    x = model_features,
    seed = training_seed,
    algorithm = "randomForest",
    grid_id = "RF_grid",
    training_frame = training_frame,
    nfolds = 5,
    hyper_params = grid_space,
    keep_cross_validation_predictions = TRUE
  )

# select best random forest model and extract cross-validated performance metrics

grid_results <-
  h2o.getGrid(grid_id = "RF_grid", decreasing = TRUE)

best_model_rf <- h2o.getModel(grid_results@model_ids[[1]])

best_cv_metrics <- best_model_rf@model$cross_validation_metrics
print(str_c(
  source_stem,
  ": best random forest model, cross-validated metrics:"
))
print(best_cv_metrics)

# train gradient boosted trees ----

grid_space$mtries <- NULL

gbm_grid <-
  h2o.grid(
    y = target_variable,
    x = model_features,
    seed = training_seed,
    algorithm = "gbm",
    grid_id = "GBM_grid",
    training_frame = training_frame,
    nfolds = 5,
    hyper_params = grid_space,
    keep_cross_validation_predictions = TRUE
  )

grid_results <-
  h2o.getGrid(grid_id = "GBM_grid", decreasing = TRUE)

best_model_gbm <- h2o.getModel(grid_results@model_ids[[1]])

best_cv_metrics <- best_model_gbm@model$cross_validation_metrics
print(str_c(
  source_stem,
  ": best gradient boosted trees model, cross-validated metrics:"
))
print(best_cv_metrics)

# blend best models of each type

selected_model <- h2o.stackedEnsemble(
  y = target_variable,
  x = model_features,
  seed = training_seed,
  training_frame = training_frame,
  base_models = list(best_model_rf, best_model_gbm)
)

# start to record facts about selected model for reporting

selected_model_info <- vector("list")

# consider performance of the best model on test data with its performance on the training set

test_set <- test_set_y %>%
  left_join(test_set_d) %>%
  left_join(test_set_x %>%
              group_by(Stim) %>%
              summarize_at(vars(starts_with("X")), mean, na.rm = TRUE)) %>%
  select(one_of(target_variable), one_of(model_features))

testing_frame <- test_set %>%
  as.h2o(destination_frame = "test")

selected_model_info$Perf <- vector("list")

selected_model_info$Perf[["training"]] <-
  h2o.performance(model = selected_model, newdata = training_frame)
selected_model_info$Perf[["test"]] <-
  h2o.performance(model = selected_model, newdata = testing_frame)

print(str_c(
  source_stem,
  ": Performance of blended model ------------------------------"
))

print("Training Frame:")
selected_model_info$Perf[["training"]] %>% print()

print("Test Frame:")
selected_model_info$Perf[["test"]] %>% print()

# consider ability of selected model to predict means of test set (note: these could be transformed targets such as Box-Cox)

means_comp_mat_test <- test_set_y %>%
  left_join(test_set_d) %>%
  select(Stim, one_of(target_variable)) %>%
  mutate(Pred = h2o.predict(selected_model, testing_frame) %>% as.vector()) %>%
  group_by(Stim) %>%
  summarize_at(c(target_variable, "Pred"), mean) %>%
  set_names(c("Stim", "Orig", "Pred"))

selected_model_info[["means_comp_mat_test"]] <-
  means_comp_mat_test

# compute predictive R^2 value

means_comp_lm_test <- means_comp_mat_test %>%
  lm(Pred ~ Orig, data = .)

means_comp_adj_r2_test <- means_comp_lm_test %>%
  summary() %>%
  .$adj.r.squared

selected_model_info[["means_comp_adj_r2_test"]] <-
  means_comp_adj_r2_test

print(str_c(
  source_stem,
  ": Test dataset, adjusted R squared for liking means = ",
  formatC(means_comp_adj_r2_test, digits = 3)
))

# plot liking mean predictions

means_comp_plot_test <- means_comp_mat_test %>%
  left_join(cleaned_keys$s) %>%
  ggplot(aes(x = Orig, y = Pred, label = Stim_Name)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggrepel::geom_label_repel(label.r = 0, label.size = 0.2, max.overlaps = 20) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
  ) +
  xlab("Liking") +
  ylab("Predicted Liking") +
  labs(
    title = str_c(
      "Predicted Liking Means, Test Dataset, Adjusted R Squared = ",
      formatC(means_comp_adj_r2_test, digits = 3, width = 5),
      sep = ""
    ),
    subtitle = "Ensemble model: Random forest and gradient boosted trees"
  )

selected_model_info[["means_comp_plot_test"]] <-
  means_comp_plot_test

# Fit selected model to full dataset and save

full_set <- yyy %>%
  left_join(ddd) %>%
  left_join(xxx %>%
              group_by(Stim) %>%
              summarize_at(vars(starts_with("X")), mean, na.rm = TRUE)) %>%
  select(one_of(target_variable), one_of(model_features))

full_frame <- full_set %>%
  as.h2o(destination_frame = "full")

full_seed <- 5432

# train full random forest

full_model_rf <- h2o.randomForest(
  y = target_variable,
  x = model_features,
  training_frame = full_frame,
  model_id = best_model_rf@model_id,
  nfolds = 5,
  seed = full_seed,
  keep_cross_validation_predictions = TRUE
)

# train full gradient boosted trees

full_model_gbm <- h2o.gbm(
  y = target_variable,
  x = model_features,
  training_frame = full_frame,
  model_id = best_model_gbm@model_id,
  nfolds = 5,
  seed = full_seed,
  keep_cross_validation_predictions = TRUE
)

# train and save full ensemble model

full_model <- h2o.stackedEnsemble(
  y = target_variable,
  x = model_features,
  seed = full_seed,
  training_frame = full_frame,
  base_models = list(full_model_rf, full_model_gbm)
)

selected_model_info$Perf[["full"]] <-
  h2o.performance(model = full_model, newdata = full_frame)

print(str_c(source_stem, ": Full dataset performance for best fitting model"))
selected_model_info$Perf[["full"]] %>% print()

# Consider liking means for full model fit

means_comp_mat_full <- yyy %>%
  left_join(ddd) %>%
  select(Stim, one_of(target_variable)) %>%
  mutate(Pred = h2o.predict(full_model, full_frame) %>% as.vector()) %>%
  group_by(Stim) %>%
  summarize_at(c(target_variable, "Pred"), mean) %>%
  set_names(c("Stim", "Orig", "Pred"))

# compute predictive R^2 value

means_comp_lm_full <- means_comp_mat_full %>%
  lm(Pred ~ Orig, data = .)

means_comp_adj_r2_full <- means_comp_lm_full %>%
  summary() %>%
  .$adj.r.squared

print(str_c(
  "Adjusted R squared for liking means using full dataset = ",
  formatC(means_comp_adj_r2_full, digits = 3)
))

# plot liking mean predictions

means_comp_plot_full <- means_comp_mat_full %>%
  left_join(cleaned_keys$s) %>%
  ggplot(aes(x = Orig, y = Pred, label = Stim_Name)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggrepel::geom_label_repel(label.r = 0) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
  ) +
  xlab("Liking") +
  ylab("Predicted Liking") +
  labs(
    title = str_c(
      "Predicted Liking Means, Full Dataset, Adjusted R Squared = ",
      formatC(means_comp_adj_r2_full, digits = 3, width = 5),
      sep = ""
    ),
    subtitle = "Ensemble model: Random forest and gradient boosted trees"
  )

selected_model_info[["means_comp_plot_full"]] <-
  means_comp_plot_full

# save the best model (and its path, which we'll need to load it later)
model_path <-
  h2o.saveModel(object = full_model,
                path = output_dir,
                force = TRUE)
selected_model_info[["model_path"]] <- model_path %>%
  basename()

# compute variable importances in final model

importance_info_rf <- full_model_rf@model$variable_importances
importance_info_gbm <- full_model_gbm@model$variable_importances

var_import_mat <- importance_info_rf %>%
  as_tibble() %>%
  select(variable, percentage) %>%
  rename(var = variable, "RF Proportion" = percentage) %>%
  left_join(
    importance_info_gbm %>%
      as_tibble() %>%
      select(variable, percentage) %>%
      rename(var = variable, "GBM Proportion" = percentage)
  ) %>%
  mutate(Source = ifelse(str_detect(var, "X"), "Panel", "Respondent"))

# plot variable importances on logarithmic scale

var_keys <- cleaned_keys$d %>%
  select(var, var_name) %>%
  bind_rows(cleaned_keys$x)

min_import_val <- 0.000001

var_import_plot <- var_import_mat %>%
  mutate(
    `RF Proportion` = pmax(`RF Proportion`, min_import_val),
    `GBM Proportion` = pmax(`GBM Proportion`, min_import_val)
  ) %>%
  left_join(var_keys) %>%
  ggplot(aes(
    x = `RF Proportion`,
    y = `GBM Proportion`,
    label = var_name,
    color = Source
  )) +
  geom_point() +
  ggrepel::geom_label_repel() +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
  ) +
  xlab("Random Forest Importance") +
  ylab("Gradient Boosted Trees Importance") +
  labs(title = "Relative Variable Importance")

selected_model_info[["var_import_mat"]] <- var_import_mat
selected_model_info[["var_import_plot"]] <- var_import_plot

# break out importance by Source and save

var_import_list <- var_import_mat %>%
  split(., .$Source)

x_import <- var_import_list$Panel %>%
  left_join(cleaned_keys$x) %>%
  select(var, var_name, "RF Proportion", "GBM Proportion")

d_import <- var_import_list$Respondent %>%
  left_join(cleaned_keys$d) %>%
  select(var, var_name, "RF Proportion", "GBM Proportion")

selected_model_info[["importance"]] <-
  list(x = x_import, d = d_import)

# save results

info_file_path <-
  file.path(output_dir,
            str_c(source_stem, "_selected_model_info.RData"))

save(selected_model_info, file = info_file_path)
