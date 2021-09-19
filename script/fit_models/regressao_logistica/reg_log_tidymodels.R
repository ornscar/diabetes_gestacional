# Logistic Regression

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
  labels = c("não", 
             "sim")
)

tab_model$ant_macrossomia <- factor(
  tab_model$ant_macrossomia, 
  labels = c("não", 
             "sim")
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
  tab_model$insulina, ref = "não")

# Selecting data --------------------

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

# Train/test data --------------------

set.seed(123)

data_split <- initial_split(
  selected_data, 
  prob = 0.75
)

data_train <- training(data_split)

data_test <- testing(data_split)

# Recipe -----------------------------

log_rec <- recipe(
  insulina ~ .,
  data = data_train
) %>%
  #step_novel(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes())

# Cross-validation -------------------

set.seed(123)

data_folds <- vfold_cv(
  data_train, 
  v = 10, 
  repeats = 5
) 

# Model specification ----------------

log_spec <- logistic_reg() %>%  
  set_engine(engine = "glm") %>%  
  set_mode("classification") 

# Workflow ---------------------------

log_workflow <- workflow() %>%
  add_model(log_spec) %>% 
  add_recipe(log_rec) 

# Resamples -------------------------

log_res <- log_workflow %>% 
  fit_resamples(
    resamples = data_folds,
    control = control_resamples(save_pred = TRUE)
) 

# Final fit model ------------------

log_fit <- log_workflow %>%
  last_fit(split = data_split)

collect_metrics(log_fit)

# Predictions ---------------------

preds <- log_fit %>% 
  collect_predictions()

summary(conf_mat(preds, insulina, .pred_class)) %>%
  dplyr::select(-.estimator) %>%
  gt() %>% 
  fmt_number(columns = 2,
             decimals = 4)

preds %>% 
  roc_curve(truth = insulina, estimate = .pred_sim) %>% 
  autoplot()

# Coefficients --------------------

log_fit$.workflow[[1]] %>%
  tidy(exponentiate = TRUE)


