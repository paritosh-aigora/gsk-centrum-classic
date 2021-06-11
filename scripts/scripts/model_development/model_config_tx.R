
glm_model_tx <- linear_reg(
  mixture = 1,
  penalty = tune()
) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

glm_model_grid_tx <- glm_model_tx %>%
  parameters() %>%
  update(penalty = penalty(range = c(0, 2), trans = NULL)) %>%
  grid_regular(levels = 50)

glm_rcp_tx <- . %>% 
  select(-any_of(c("consid", "product"))) %>%
  recipe(formula = Target ~ .) %>%
  step_zv(all_predictors()) %>%
  step_impute_median(all_numeric(), -all_outcomes()) %>%
  step_YeoJohnson(all_predictors())


# Model config to tibble
model_configs_tx <- list(
  GLMNet_num_tx = list(
    model = glm_model_tx,
    grid = glm_model_grid_tx,
    pre_rcp = glm_rcp_tx,
    type = "num"
  )
) %>% 
  map(enframe) %>%
  enframe("model_name") %>%
  unnest("value") %>%
  pivot_wider() %>%
  mutate(type = unlist(type))
