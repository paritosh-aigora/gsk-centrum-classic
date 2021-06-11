# GLMNet
glm_model <- linear_reg(
  mixture = tune(),
  penalty = tune()
) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

glm_model_grid <- glm_model %>%
  parameters() %>%
  update(mixture = mixture(c(0, 1))) %>%
  grid_regular(levels = 10)

glm_rcp <- . %>% 
  select(-any_of(c("consid", "product"))) %>%
  recipe(formula = Target ~ .) %>%
  step_novel(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
  step_impute_median(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
  step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
  step_nzv(all_predictors(), freq_cut = 5/1) %>%
  step_earth(all_numeric(), -all_outcomes(), outcome = "Target", drop = FALSE,
             options = list(degree = 2))

# GLMNet
mult_glm_model <- multinom_reg(
  mixture = tune(),
  penalty = tune()
) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

mult_glm_model_grid <- mult_glm_model %>%
  parameters() %>%
  update(mixture = mixture(c(0, 1))) %>%
  grid_regular(levels = 10)

mult_glm_rcp <- . %>% 
  select(-any_of(c("consid", "product"))) %>%
  recipe(formula = Target ~ .) %>%
  step_novel(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
  step_impute_median(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
  step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
  step_nzv(all_predictors(), freq_cut = 5/1) %>%
  step_earth(all_numeric(), -all_outcomes(), outcome = "Target", drop = FALSE,
             options = list(degree = 2))

# ordinal_reg

ord_model <- ordinal_reg(
  mixture = tune(),
  penalty = tune()
) %>%
  set_mode("classification") %>%
  set_engine("glmnetcr")

ord_model_grid <- ord_model %>%
  parameters() %>%
  update(mixture = mixture(c(0, 1)),
         penalty = penalty(c(0.01, 0.1), trans = NULL)) %>%
  grid_regular(levels = 10)


# Model config to tibble
model_configs <- list(
  GLMNet_num_earth2 = list(
    model = glm_model,
    grid = glm_model_grid,
    pre_rcp = glm_rcp,
    type = "num"
  ),
  GLMNet_mult_earth2 = list(
    model = mult_glm_model,
    grid = mult_glm_model_grid,
    pre_rcp = mult_glm_rcp,
    type = "jar"
  )
  # ,
  # ord_earth2 = list(
  #   model = ord_model,
  #   grid = ord_model_grid,
  #   pre_rcp = glm_rcp,
  #   type = "jar"
  # )
) %>% 
  map(enframe) %>%
  enframe("model_name") %>%
  unnest("value") %>%
  pivot_wider() %>%
  mutate(type = unlist(type))

