# SHAP

# Libraries ----------------------------

library(tidymodels)
library(janitor)
library(probably)
library(vip)
library(gt)
library(reticulate)
library(fastshap)

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

data_test <- testing(data_split)

# Recipe ----------------------------------

rec_data <- recipe(insulina ~ ., data = data_train) %>%
  step_dummy(
    imc_classe, 
    diabetes_familia, 
    macrossomia_fetal, 
    diabetes_gestacional, 
    tabagista, 
    hipertensao
  )

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
  add_recipe(rec_data) %>%
  add_model(xgb_spec) %>% 
  fit(data_train)

# Interpretability --------------------------

# data

X <- prep(rec_data, data_train) %>% 
  juice() %>% 
  dplyr::select(-insulina) %>% 
  as.matrix()

# fitted model

xgb_model <- pull_workflow_fit(xgb_wf)

xgb_fit <- xgb_model$fit

saveRDS(xgb_fit, "xgb_fit.rds")

# shapley values 

shap <- explain(
  object = xgb_model$fit, 
  X = X, 
  exact = TRUE
)

# graph

source("util/plots/force_plot_bruno.R")

force_plot_bruno(
  object = shap[1, ], 
  feature_values = X[1, ], 
  display = "viewer", 
  link = "logit"
)

