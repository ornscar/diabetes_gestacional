# Linear Discriminant Analysis

# Libraries -----------------------

library(tidymodels)
library(janitor)
library(discrim)
library(klaR)
library(nonlinearTseries)
library(doParallel)
library(vip)
library(probably)
library(gt)

# Loading data --------------------

tab_model <- readr::read_delim(
  "data/data_model.csv", 
  ";", 
  escape_double = FALSE, 
  trim_ws = TRUE
)

# Tidying data ---------------------

tab_model <- tab_model %>%
  clean_names()

tab_model <- rename(
  tab_model, 
  n_gestacoes = g, 
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

tab_model$familia_diabetes <- factor(
  tab_model$familia_diabetes, 
  labels = c("não", "sim")
)

tab_model$ant_macrossomia <- factor(
  tab_model$ant_macrossomia, 
  labels = c("não", "sim")
)

tab_model$pessoal_de_dmg <- factor(
  tab_model$pessoal_de_dmg, 
  labels = c("primigesta", 
             "não", 
             "sim")
)

tab_model$pessoal_de_dmg <- forcats::fct_recode(
  tab_model$pessoal_de_dmg, 
  nao = "primigesta",
  nao = "não",
  sim = "sim"
)

tab_model$tabagismo <- factor(
  tab_model$tabagismo, 
  labels = c("não", "sim")
)

tab_model$hipertensao <- factor(
  tab_model$hipertensao, 
  levels = c("0", "1"),
  labels = c("não", "sim")
)

tab_model$insulina <- factor(
  tab_model$insulina, 
  labels = c("não", "sim")
)

tab_model$insulina <- with(
  tab_model, 
  factor(insulina, levels = c("sim","não"))
)

# Selecting data ------------------

selected_feat <- tab_model %>% 
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
)

selected_data <- selected_feat[complete.cases(selected_feat) == TRUE, ]

# Train/test data -------------------

set.seed(123)

data_split <- initial_split(selected_data, prob = 0.75)
data_train <- training(data_split)
data_test <- testing(data_split)

# Recipe ----------------------------

data_rec <- recipe(insulina ~ ., data = data_train) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes())

data_rec %>% 
  prep() %>% 
  bake(new_data = data_train)

# Model specification ---------------

lda_spec <- discrim_regularized(
  frac_common_cov = tune()) %>% 
  set_engine("klaR") %>% 
  set_mode('classification')

# Workflow --------------------------

lda_wf <- workflow() %>% 
  add_model(lda_spec) %>% 
  add_recipe(data_rec)

# Cross-validation ------------------

set.seed(123)

data_folds <- vfold_cv(
  data_train, 
  v = 10, 
  repeats = 5
) 

# Grid search -----------------------

lda_grid <- lda_spec %>% 
  param_set() %>% 
  grid_max_entropy(size = 30)

lda_grid

# Tunning --------------------------

doParallel::registerDoParallel()

set.seed(234)

roc_values <- metric_set(roc_auc)

lda_res <- tune_grid( 
  lda_wf,
  resamples = data_folds,
  grid = lda_grid,
  control = control_grid(verbose = TRUE),
  perf = roc_values
)

lda_res

# Best parameters/performance -----

collect_metrics(lda_res) %>% arrange(desc(mean)) 

best_auc <- select_best(lda_res, "roc_auc"); best_auc

# Best LDA model ------------------

final_lda <- finalize_workflow(
  lda_wf,
  best_auc
)

# Fit model -----------------------

final_lda %>%
  fit(data = data_train) %>%
  pull_workflow_fit()

# Final fit model -----------------

final_res <- last_fit(final_lda, data_split)

collect_metrics(final_res)

# Predictions / confusion matrix --

preds <- final_res %>% 
  collect_predictions()

conf_mat(preds, insulina, .pred_class)

summary(conf_mat(preds, insulina, .pred_class)) %>%
  dplyr::select(-.estimator) %>%
  gt() 


