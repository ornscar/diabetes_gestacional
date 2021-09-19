# XGBoost

# Libraries ----------------------------

library(tidymodels)
library(janitor)
library(probably)
library(vip)
library(gt)

# Loading data -------------------------

tab_model <- readr::read_csv2(
  "data/data_model.csv"
)

# Tidying data -------------------------

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

# Selecting data -----------------------

selected_data <- tab_model %>% 
  dplyr::select(
    idade, 
    n_gestacoes, 
    imc_classe,
    diabetes_familia, 
    macrossomia_fetal, 
    diabetes_gestacional,
    tabagista, 
    hipertensao,
    glicemia_jejum, 
    insulina
  ) %>% 
  drop_na()

# Train/test data ------------------------

set.seed(807189364)

data_split <- initial_split(
  selected_data, 
  prob = 0.75
)

data_train <- training(data_split)

saveRDS(data_train, "train_data.rds")

data_test <- testing(data_split)

# Model specification ---------------------

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = 9, min_n = 5,
  loss_reduction = 0.00000461,     ## first three: model complexity
  sample_size = 0.640, mtry = 7,   ## randomness
  learn_rate = 0.00209,            ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Workflow -----------------------------------

xgb_wf <- workflow() %>%
  add_formula(insulina ~ .) %>%
  add_model(xgb_spec) %>% 
  fit(data_train)

saveRDS(xgb_wf, "fitted_xgb.rds")

# Final fit ----------------------------------

xgb_res <- last_fit(
  xgb_wf,
  data_split,
  metrics = metric_set(roc_auc, accuracy)
) 

collect_metrics(xgb_res)

# Predictions / confusion matrix -------------

preds <- xgb_res %>% 
  collect_predictions()

summary(conf_mat(preds, insulina, .pred_class)) %>%
  dplyr::select(-.estimator) %>%
  gt() %>% 
  fmt_number(columns = 2,
             decimals = 4)

# Defining a threshold -----------------------

thresholds <- preds %>%
  threshold_perf(insulina, .pred_sim, thresholds = seq(0, 1, by = 0.0025))

best_thresh <- thresholds %>%
  filter(.metric == "j_index") %>%
  filter(.estimate == max(.estimate)) %>%
  pull(.threshold) %>% 
  max()

preds_new <- preds %>% 
  mutate(.new_pred_class = factor(ifelse(.pred_sim >= best_thresh, "sim", "não"), 
                                  levels = c("sim", "não")))

# Comparing models ----------------------------

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


