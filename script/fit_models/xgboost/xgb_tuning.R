# XGBoost - Tuning

# Libraries -------------------------

library(tidymodels)
library(janitor)
library(vip)
library(probably)
library(gt)

# Loading data ----------------------

tab_model <- readr::read_csv2(
  "data/data_model.csv"
)

# Tidying data ----------------------

tab_model <- tab_model %>%
  clean_names()

tab_model <- dplyr::rename(
  tab_model, 
  n_gestacoes = g, 
  diabetes_familia = familia_diabetes,
  macrossomia_fetal = ant_macrossomia,
  diabetes_gestacional = pessoal_de_dmg,
  tabagista = tabagismo,
  hipertensao = hac,
  glicemia_jejum = glicema_jejum
)

tab_model <- tab_model %>% 
  mutate(ano = gsub("[.]","", ano))

tab_model$imc_classe <- factor(
  tab_model$imc_classe, 
  labels = c("até normal",
             "sobrepeso", 
             "obeso")
)

tab_model$diabetes_familia <- factor(
  tab_model$diabetes_familia, 
  labels = c("não", 
             "sim")
)

tab_model$macrossomia_fetal <- factor(
  tab_model$macrossomia_fetal, 
  labels = c("não", 
             "sim")
)

tab_model$diabetes_gestacional <- factor(
  tab_model$diabetes_gestacional, 
  labels = c("primigesta", 
             "não", 
             "sim")
)

tab_model$diabetes_gestacional <- forcats::fct_recode(
  tab_model$diabetes_gestacional, 
  nao = "primigesta",
  nao = "não",
  sim = "sim"
)

tab_model$tabagista <- factor(
  tab_model$tabagista, 
  labels = c("não", 
             "sim")
)

tab_model$hipertensao <- factor(
  tab_model$hipertensao, 
  levels = c("0", "1"),
  labels = c("não", 
             "sim")
)

tab_model$insulina <- factor(
  tab_model$insulina, 
  labels = c("não", 
             "sim")
)

tab_model$insulina <- relevel(
  tab_model$insulina, 
  ref = "sim"
)

# Selecting data -------------------

selected_data <- tab_model %>% 
  dplyr::select(
    idade, 
    n_gestacoes, 
    imc_classe,
    familia_diabetes, 
    ant_macrossomia, 
    pessoal_de_dmg,
    tabagismo, 
    hipertensao,
    glicemia_jejum, 
    insulina
  ) %>% 
  drop_na()

# Train/test data ------------------

set.seed(456)

data_split <- initial_split(
  selected_data, 
  prob = 0.75
)

data_train <- training(data_split)

data_test <- testing(data_split)

# Recipe ---------------------------

# rec_data <- recipe(
#   insulina ~ .,
#   data = data_train
# ) 
# %>% 
#   step_corr(all_numeric()) %>%
#   step_zv(all_predictors()) %>%
#   step_normalize(all_numeric()) %>%
#   step_dummy(all_nominal(), -all_outcomes())

# Preparing data -------------------

# prepar_data <- rec_data %>% 
#   prep() %>%                  ## perform the recipe on training data
#   bake(new_data = NULL)

# Model specification --------------

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# Grid search ----------------------

xgb_grid <- grid_max_entropy(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), data_train),
  learn_rate(),
  size = 60
)

# Workflow -------------------------

xgb_wf <- workflow() %>%
  add_model(xgb_spec) %>% 
  add_recipe(rec_data)

# Cross-validation -----------------

set.seed(789)

data_folds <- vfold_cv(
  data_train, 
  v = 10, 
  repeats = 5
)   

# Tuning -----------------------------

doParallel::registerDoParallel()

set.seed(1011)

xgb_res <- tune_grid(
  xgb_wf,
  resamples = data_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

# Best hyperparameters/performance ---

collect_metrics(xgb_res)

show_best(xgb_res, "roc_auc")

best_auc <- select_best(xgb_res, "roc_auc"); best_auc  

# Best Boosted Tree model ------------

final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc
)

# Final fit model --------------------

final_fit <- last_fit(
  final_xgb, 
  data_split,
)

collect_metrics(final_fit)

# Predictions / confusion matrix ---

conf_mat(
  data = final_fit$.predictions[[1]],
  truth = insulina,
  estimate = .pred_class
) %>% 
  autoplot(type = "heatmap")

preds <- final_fit %>% 
  collect_predictions() 

summary(conf_mat(preds, insulina, .pred_class)) %>%
  dplyr::select(-.estimator) %>%
  gt() %>% 
  fmt_number(columns = 2,
             decimals = 4)

# Defining a threshold -------------

thresholds <- preds %>%
  threshold_perf(insulina, .pred_sim, thresholds = seq(0, 1, by = 0.0025))

best_thresh <- thresholds %>%
  filter(.metric == "j_index") %>%
  filter(.estimate == max(.estimate)) %>%
  pull(.threshold)

preds_new <- preds %>% 
  mutate(.new_pred_class = factor(ifelse(.pred_sim >= best_thresh, "sim", "não"), 
                                  levels = c("sim", "não")))

# Comparing models -----------------

summary(conf_mat(preds, insulina, .pred_class)) %>%
  dplyr::select(-.estimator) %>%
  rename(old_threshold = .estimate) %>%
  bind_cols(.,
            summary(conf_mat(preds_new, insulina, .new_pred_class)) %>%
              dplyr::select(.estimate) %>%
              rename(new_threshold = .estimate)) %>% 
  gt() %>%
  fmt_number(columns = c(2, 3),
             decimals = 4)


