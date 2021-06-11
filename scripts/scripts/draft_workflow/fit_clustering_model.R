
# train clustering model --------------------------------------------------

# set training proportion
train_prop <- 1

# define parameters ---

# length of the grid searches (impacts computing time)
grid_search_length <- 10

# number of folds used in cross-validation
folds <- 10

# number of repeats used in cross-validation
repeats <- 3

#for (source_stem in source_stems_to_run) {
print(str_c("Fitting: ", source_stem, " ------------------------------"))

output_dir <- file.path("output", "modeling_results", source_stem)

if (!dir.exists(output_dir))
  dir.create(output_dir)

load(file.path(
  "input",
  "cleaned_data",
  str_c(source_stem, "_cleaned_data.Rdata")
))

# read in data (predictors are D variable, class is Cluster)
class_data <- cleaned_data$d %>%
  mutate_if(is.ordered, factor, ordered = FALSE) %>%
  select(-ID_Y) %>%
  rename(Class = Cluster) %>%
  select(Class, starts_with("D"))

num_pred_vars <- class_data %>% 
  select(starts_with("D")) %>% 
  ncol()

# Oversampling using ROSE: set number of outcome rows to twice the biggest class size
nrows <- max(summary(class_data$Class)) * 2

# seed
seed <- 34

data_over <-
  ROSE::ROSE(Class ~ .,
             data = class_data,
             N = nrows,
             seed = seed)$data

# Prepare data ----

# Make factors numerics
data_over_num <- data_over %>%
  mutate_at(vars(starts_with("D")), as.numeric)

# Transform non-oversampled data as well
data_num <- class_data %>%
  mutate_all(as.numeric)

# create splits and recipes -----

# Set up cross validations for both original data and numeric data
set.seed(seed)

cv_splits <- vfold_cv(data_over, v = folds, repeats = repeats)

cv_splits_num <-
  vfold_cv(data_over_num, v = folds, repeats = repeats)

# Make recipes for both original data and numeric data
recipe <-
  recipe(Class ~ ., data = data_over) %>%
  prep()

recipe_num <-
  recipe(Class ~ ., data = data_over_num) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  prep()

# run models -----

# neural network ----

nnet <- mlp(
  mode = "classification",
  epochs = tune("E"),
  hidden_units = tune("HU"),
  penalty = tune("P")
) %>%
  set_engine("nnet")

nnet_wflow <- workflow() %>%
  add_recipe(recipe_num) %>%
  add_model(nnet)

nnet_set <-
  parameters(nnet_wflow) %>%
  update(E = epochs(c(500, 1500))) %>%
  update(HU = hidden_units(c(1, 10))) %>%
  update(P = penalty(c(0, -1)))

nnet_grid <-
  nnet_set %>%
  grid_max_entropy(size = grid_search_length)

# runtime is about 2 to 3 minutes
set.seed(seed)
nnet_grid_search <-
  tune_grid(
    nnet_wflow,
    resamples = cv_splits_num,
    grid = nnet_grid,
    metrics = metric_set(bal_accuracy, roc_auc)
  )

show_best(nnet_grid_search, metric = "bal_accuracy")

show_best(nnet_grid_search, metric = "roc_auc")

# select best model
nnet_param_best <-
  select_best(nnet_grid_search, metric = "bal_accuracy")

nnet_model_best <- finalize_model(nnet, nnet_param_best)

nnet_model_finalfit <-
  fit(nnet_model_best, Class ~ ., data = data_over_num)

# random forest ----

rf_mod <-
  rand_forest(
    mode = "classification",
    mtry = tune("m"),
    trees = tune("t"),
    min_n = tune("mn")
  ) %>%
  set_engine("ranger", importance = "permutation")

rf_wflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(rf_mod)

rf_set <-
  parameters(rf_wflow) %>%
  update(m = mtry(c(0, min(num_pred_vars, 5))),
         t = trees(c(5, 150)),
         mn = min_n(c(1, 10)))

rf_grid <-
  rf_set %>%
  grid_max_entropy(size = grid_search_length)

set.seed(seed)
rf_grid_search <-
  tune_grid(
    rf_wflow,
    resamples = cv_splits,
    grid = rf_grid,
    metrics = metric_set(bal_accuracy, roc_auc)
  )

show_best(rf_grid_search, metric = "bal_accuracy")

show_best(rf_grid_search, metric = "roc_auc")

autoplot(rf_grid_search, metric = "bal_accuracy")

# select best model
rf_param_best <-
  select_best(rf_grid_search, metric = "bal_accuracy")

rf_model_best <- finalize_model(rf_mod, rf_param_best)

rf_model_finalfit <-
  fit(rf_model_best,
      Class ~ .,
      data = data_over,
      importance = "permutation")

rf_conf_data <-
  data.frame(
    pred_values = predict(rf_model_finalfit, new_data = data_over),
    actual_values = data_over$Class
  )

conf_mat(rf_conf_data, truth = "actual_values", estimate = ".pred_class")

# boosted trees -----

boost_mod <-
  boost_tree(
    mode = "classification",
    mtry = tune("m"),
    trees = tune("t"),
    min_n = tune("mn"),
    learn_rate = tune("lr")
  ) %>%
  set_engine("xgboost")

boost_wflow <- workflow() %>%
  add_recipe(recipe_num) %>%
  add_model(boost_mod)

boost_set <-
  parameters(boost_wflow) %>%
  update(
    m = mtry(c(0, min(num_pred_vars, 5))),
    t = trees(c(5, 150)),
    mn = min_n(c(1, 10)),
    lr = learn_rate(c(-1, 0))
  )

boost_grid <-
  boost_set %>%
  grid_max_entropy(size = grid_search_length)

set.seed(seed)
boost_grid_search <-
  tune_grid(
    boost_wflow,
    resamples = cv_splits_num,
    grid = boost_grid,
    metrics = metric_set(bal_accuracy, roc_auc)
  )

show_best(boost_grid_search, metric = "bal_accuracy")

show_best(boost_grid_search, metric = "roc_auc")

autoplot(boost_grid_search, metric = "bal_accuracy")

# select best model
boost_param_best <-
  select_best(boost_grid_search, metric = "bal_accuracy")

boost_model_best <- finalize_model(boost_mod, boost_param_best)

boost_model_finalfit <-
  fit(boost_model_best, Class ~ ., data = data_over_num)

# glmnet ----

lr_model <- logistic_reg(mode = "classification",
                         penalty = tune("P"),
                         mixture = tune("M")) %>%
  set_engine("glmnet")

lr_model_wflow <- workflow() %>%
  add_recipe(recipe_num) %>%
  add_model(lr_model)

lr_model_set <-
  parameters(lr_model_wflow) %>%
  update(P = penalty(c(-1, 0)),
         M = mixture(c(0, 1)))

lr_model_grid <-
  lr_model_set %>%
  grid_max_entropy(size = grid_search_length)

set.seed(seed)
lr_model_grid_search <-
  tune_grid(
    lr_model_wflow,
    resamples = cv_splits_num,
    grid = lr_model_grid,
    metrics = metric_set(bal_accuracy, roc_auc)
  )

show_best(lr_model_grid_search, metric = "bal_accuracy")

show_best(lr_model_grid_search, metric = "roc_auc")

autoplot(lr_model_grid_search, metric = "bal_accuracy")

# select best model & get confusion matrix

lr_model_param_best <-
  select_best(lr_model_grid_search, metric = "bal_accuracy")

lr_model_best <- finalize_model(lr_model, lr_model_param_best)

lr_model_finalfit <-
  fit(lr_model_best, Class ~ ., data = data_over_num)

# pick top models and blend in an ensemble

# Compare model performance & pick the top two performers to create stacked ensemble
model_performance <-
  data.frame(
    model = c("nnet", "random_forest", "boost_tree", "glm"),
    bal_accuracy = c(
      show_best(nnet_grid_search, metric = "bal_accuracy")$mean[1],
      show_best(rf_grid_search, metric = "bal_accuracy")$mean[1],
      show_best(boost_grid_search, metric = "bal_accuracy")$mean[1],
      show_best(lr_model_grid_search, metric = "bal_accuracy")$mean[1]
    )
  ) %>%
  dplyr::arrange(desc(bal_accuracy)) %>%
  dplyr::top_n(2, bal_accuracy)

# create ensemble data

feature_data <- data_over_num %>%
  select(-Class)

ensemble_data <- data.frame(
  Class = data_over$Class,
  X1 = predict(rf_model_finalfit, feature_data, type = "prob")[, 2],
  X2 = predict(nnet_model_finalfit, feature_data, type = "prob")[, 2],
  X3 = predict(boost_model_finalfit, feature_data, type = "prob")[, 2],
  X4 = predict(lr_model_finalfit, feature_data, type = "prob")[, 2]
)

names(ensemble_data) <-
  c("Class", "random_forest", "nnet", "boost_tree", "glm")

ensemble_data <- ensemble_data %>%
  select("Class", model_performance$model)

# Make recipe
recipe_ensemble <- recipe(Class ~ ., data = ensemble_data) %>%
  prep()

# Set up cross validation: 10-fold with 3 repeats
set.seed(seed)
cv_splits_ensemble <-
  vfold_cv(ensemble_data, v = folds, repeats = repeats)

# Base learner: logistic regression (this is also a saved object)

lr <- logistic_reg(mode = "classification",
                   penalty = tune("P"),
                   mixture = tune("M")) %>%
  set_engine("glmnet")

lr_wflow <- workflow() %>%
  add_recipe(recipe_ensemble) %>%
  add_model(lr)

lr_set <-
  parameters(lr_wflow) %>%
  update(P = penalty(c(-1, 0)),
         M = mixture(c(0, 1)))

lr_grid <-
  lr_set %>%
  grid_max_entropy(size = grid_search_length)

set.seed(seed)
lr_grid_search <-
  tune_grid(
    lr_wflow,
    resamples = cv_splits_ensemble,
    grid = lr_grid,
    metrics = metric_set(bal_accuracy, roc_auc)
  )

show_best(lr_grid_search, metric = "bal_accuracy")

show_best(lr_grid_search, metric = "roc_auc")

autoplot(lr_grid_search, metric = "bal_accuracy")

# select best model & get confusion matrix -----

lr_param_best <-
  select_best(lr_grid_search, metric = "bal_accuracy")

lr_best <- finalize_model(lr, lr_param_best)

lr_finalfit <- fit(lr_best, Class ~ ., data = ensemble_data)

conf_data <-
  data.frame(
    pred_values = predict(lr_finalfit, new_data = ensemble_data),
    actual_values = ensemble_data$Class
  )

conf_mat(conf_data, truth = "actual_values", estimate = ".pred_class")

# explain model ----

if (names(ensemble_data)[2] == "random_forest") {
  model1 <- rf_model_finalfit
}

if (names(ensemble_data)[2] == "nnet") {
  model1 <- nnet_model_finalfit
}

if (names(ensemble_data)[2] == "boost_tree") {
  model1 <- boost_model_finalfit
}

if (names(ensemble_data)[2] == "glm") {
  model1 <- lr_model_finalfit
}

if (names(ensemble_data)[3] == "random_forest") {
  model2 <- rf_model_finalfit
}

if (names(ensemble_data)[3] == "nnet") {
  model2 <- nnet_model_finalfit
}

if (names(ensemble_data)[3] == "boost_tree") {
  model2 <- boost_model_finalfit
}

if (names(ensemble_data)[3] == "glm") {
  model2 <- lr_model_finalfit
}

# 1. First model

if (names(ensemble_data)[2] == "random_forest") {
  model1_data <- class_data %>%
    mutate(Class = as.numeric(Class))
} else {
  model1_data <- data_num
}

model1_explainer <- DALEX::explain(
  model1,
  data = model1_data[, -1],
  y = model1_data$Class,
  type = "classification",
  label = names(ensemble_data)[2]
)

bd_model1 <- break_down(
  model1_explainer,
  model1_data[which(model1_data$Class == 1)[1],-1],
  keep_distributions = TRUE,
  check_interactions = FALSE
)

plot(bd_model1) +
  ggtitle(paste0("Class label A prediction for ", names(ensemble_data[2])))

bd_model1.2 <- break_down(
  model1_explainer,
  model1_data[which(model1_data$Class == 2)[1],-1],
  keep_distributions = TRUE,
  check_interactions = FALSE
)

plot(bd_model1.2) +
  ggtitle(paste0("Class label B prediction for ", names(ensemble_data[2])))

# 2. Second model

if (names(ensemble_data)[3] == "random_forest") {
  model2_data <- class_data %>%
    mutate(Class = as.numeric(Class))
} else {
  model2_data <- data_num
}

model2_explainer <- DALEX::explain(
  model2,
  data = model2_data[,-1],
  y = model2_data$Class,
  type = "classification",
  label = names(ensemble_data)[3]
)

bd_model2 <- breakDown::break_down(
  model2_explainer,
  model2_data[which(model2_data$Class == 1)[1],-1],
  keep_distributions = TRUE,
  check_interactions = FALSE
)

plot(bd_model2) +
  ggtitle(paste0("Class label A prediction for ", names(ensemble_data[3])))

bd_model2.2 <- breakDown::break_down(
  model2_explainer,
  model2_data[which(model2_data$Class == 2)[1],-1],
  keep_distributions = TRUE,
  check_interactions = FALSE
)

plot(bd_model2.2) +
  ggtitle(paste0("Class label B prediction for ", names(ensemble_data[3])))


# 3. Ensemble: the final model

ensemble_explainer <- DALEX::explain(
  lr_finalfit,
  data = ensemble_data[,-1],
  y = ensemble_data$Class %>% as.character(),
  type = "classification",
  label = "Ensemble"
)

bd_ensemble <- breakDown::break_down(
  ensemble_explainer,
  ensemble_data[which(ensemble_data$Class == 1)[1],-1],
  keep_distributions = TRUE,
  check_interactions = FALSE
)

plot(bd_ensemble) +
  ggtitle("Class label A prediction (ensemble)")


bd_ensemble2 <- breakDown::break_down(
  ensemble_explainer,
  ensemble_data[which(ensemble_data$Class == 2)[1],-1],
  keep_distributions = TRUE,
  check_interactions = FALSE
)

plot(bd_ensemble2) +
  ggtitle("Class label B prediction (ensemble)")


# Performance of the ensemble model (not cross validated):

ensemble_explainer2 <- DALEX::explain(
  lr_finalfit,
  data = ensemble_data[,-1],
  y = ifelse(ensemble_data$Class == 1, 0, 1),
  type = "classification",
  label = "Ensemble"
)

DALEX::model_performance(ensemble_explainer2, cutoff = 0.5)

# variable importance -----


# Variable importance for model 1
vip_model1 <- DALEX::variable_importance(
  model1_explainer,
  loss_function = function(observed, predicted)
    bal_accuracy_vec(truth = as.factor(observed), estimate = as.factor(ifelse(predicted > 0.5, 2, 1))),
  type = "ratio"
)

plot(vip_model1)

vip_1 <- vip_model1 %>%
  as_tibble() %>%
  group_by(variable) %>%
  summarize(dropout_loss = mean(dropout_loss)) %>%
  ungroup() %>%
  inner_join(cleaned_keys$d %>% select(var, var_name),
             by = c("variable" = "var")) %>%
  arrange(dropout_loss) %>%
  mutate(dropout_loss = ifelse(dropout_loss > 1, 1, dropout_loss)) %>%
  mutate(loss = 1 - dropout_loss) 

if (sum(vip_1$loss) == 0){
  vip_1 <- vip_1 %>% 
    mutate(importance = 1/ nrow(.)) %>% 
    select(var_name, importance)
} else {
  
  vip_1 <- vip_1 %>% 
    mutate(importance = loss / sum(loss)) %>% 
    select(var_name, importance)
}

# Variable importance for model 2
vip_model2 <- DALEX::variable_importance(
  model2_explainer,
  loss_function = function(observed, predicted)
    bal_accuracy_vec(truth = as.factor(observed), estimate = as.factor(ifelse(predicted > 0.5, 2, 1)))
)

plot(vip_model2)

vip_2 <- vip_model2 %>%
  as_tibble() %>%
  group_by(variable) %>%
  summarize(dropout_loss = mean(dropout_loss)) %>%
  ungroup() %>%
  inner_join(cleaned_keys$d %>% select(var, var_name),
             by = c("variable" = "var")) %>%
  arrange(dropout_loss) %>%
  mutate(dropout_loss = ifelse(dropout_loss > 1, 1, dropout_loss)) %>%
  mutate(loss = 1 - dropout_loss) 

if (sum(vip_2$loss) == 0){
  vip_2 <- vip_2 %>% 
    mutate(importance = 1/ nrow(.)) %>% 
    select(var_name, importance)
} else {

  vip_2 <- vip_2 %>% 
    mutate(importance = loss / sum(loss)) %>% 
    select(var_name, importance)
}

# Variable importance for ensemble

vip_ensemble <- DALEX::variable_importance(
  ensemble_explainer,
  loss_function = function(observed, predicted)
    bal_accuracy_vec(truth = as.factor(observed), estimate = as.factor(ifelse(predicted > 0.5, 2, 1)))
)

plot(vip_ensemble)

ens_vip <- vip_ensemble %>%
  as_tibble() %>%
  group_by(variable) %>%
  summarize(dropout_loss = mean(dropout_loss)) %>%
  filter(!(variable %in% c("_baseline_", "_full_model_"))) %>%
  arrange(dropout_loss) %>%
  mutate(dropout_loss = ifelse(dropout_loss > 1, 1, dropout_loss)) %>%
  mutate(loss = 1 - dropout_loss) %>%
  mutate(importance = loss / sum(loss)) %>%
  rename(model = variable) %>%
  select(model, importance) %>% 
  rename(type = model) %>% 
  mutate(model = c("model_1", "model_2"))

# plot results

joint_vip <- vip_1 %>%
  rename(model_1 = importance) %>%
  left_join(vip_2 %>% rename(model_2 = importance))

max_vip <- joint_vip %>% 
  select(-var_name) %>% 
  max()

# joint_vip %>%
#   ggplot(aes(x = model_1, y = model_2)) +
#   geom_point() +
#   geom_label(aes(label = var_name)) +
#   scale_x_continuous(limits = c(0, max_vip)) +
#   scale_y_continuous(limits = c(0, max_vip))

# combine results

full_vip <- joint_vip %>%
  pivot_longer(-var_name, names_to = "model", values_to = "vip") %>%
  left_join(ens_vip %>% rename(weight = importance)) %>%
  mutate(w_vip = weight * vip) %>%
  group_by(var_name) %>%
  summarize(r_vip = sum(w_vip)) %>%
  mutate(vip = r_vip / sum(r_vip)) %>%
  select(var_name, vip) %>%
  arrange(desc(vip))

# save results

vip_info <-
  list(
    ens = ens_vip,
    joint = joint_vip,
    full = full_vip
  )

info_file_path <-
  file.path(output_dir, str_c(source_stem, "_cluster_vip.RData"))

save(vip_info, file = info_file_path)
