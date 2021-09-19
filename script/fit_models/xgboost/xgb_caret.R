# XGBoost

# Libraries -----------------------

library(dplyr)
library(magrittr)
library(janitor)
library(caret)

# Loading data --------------------

dados_modelo <- readr::read_delim(
  "data/data_model.csv", 
  ";", 
  escape_double = FALSE, 
  trim_ws = TRUE
)

# Tidying data ---------------------

dados_modelo <- dados_modelo %>% 
  clean_names()

dados_modelo <- rename(
  dados_modelo, 
  n_gestacoes = g, 
  hipertensao = hac,
  glicemia_jejum = glicema_jejum
)

dados_modelo <- dados_modelo %>% 
  mutate(ano = gsub("[.]","", ano))

dados_modelo$pessoal_de_dmg <- factor(
  dados_modelo$pessoal_de_dmg, 
  labels = c("primigesta", 
             "não", 
             "sim")
)

dados_modelo$pessoal_de_dmg <- forcats::fct_recode(
  dados_modelo$pessoal_de_dmg, 
  nao = "primigesta",
  nao = "não",
  sim = "sim"
)

dados_modelo$insulina <- factor(
  dados_modelo$insulina, 
  labels = c("não", "sim")
)
                             
# Selected data --------------------

vas_selecionadas <- dados_modelo %>% 
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

# remove NA

dados_sem_na <- vas_selecionadas[complete.cases(vas_selecionadas) == TRUE, ]

# Scaled data --------------------

dados_selecionados <- dados_sem_na %>%
  select(idade, n_gestacoes, glicemia_jejum) %>% 
  mutate_if(is.numeric, scale) %>% 
  mutate(imc_classe = dados_sem_na$imc_classe,
         familia_diabetes = dados_sem_na$familia_diabetes,
         ant_macrossomia = dados_sem_na$ant_macrossomia,
         pessoal_de_dmg = dados_sem_na$pessoal_de_dmg,
         tabagismo = dados_sem_na$tabagismo,
         hipertensao = dados_sem_na$hipertensao) %>% 
  mutate_if(is.factor, as.numeric) %>% 
  mutate(insulina = dados_sem_na$insulina) %>% 
  as_tibble()

# Train/test data -----------------

set.seed(1234)

particionando_dados <- createDataPartition(
  y = dados_selecionados$insulina, 
  p = 0.75, 
  list = FALSE
) 

dados_treino <- dados_selecionados[particionando_dados, ]

dados_teste <- dados_selecionados[-particionando_dados, ]

# Train/test dmatrices ------------

X_treino <- as.matrix(dados_treino %>% 
                        dplyr::select(-insulina))
y_treino <- dados_treino$insulina

X_teste <- as.matrix(dados_teste %>% 
                       dplyr::select(-insulina))
y_teste <- dados_teste$insulina

# Cross-validation ----------------

xgb_control <- trainControl(
  method = "repeatedcv",
  number = 10, 
  repeats = 5, 
  search = "grid",
  allowParallel = TRUE
)

# Grid search ---------------------

xgb_grid <- expand.grid(
  nrounds = c(100, 200),  
  max_depth = c(3, 5, 8),
  colsample_bytree = c(0.5, 1),
  eta = c(0.01, 0.2),
  gamma = c(0, 0.05),
  min_child_weight = c(1, 2),
  subsample = c(0.5, 1)
  )

# Tuning/fit model ----------------

set.seed(1234)

xgb_ajuste <- train(
  X_treino, y_treino,
  method = "xgbTree",
  trControl = xgb_control,
  tuneGrid = xgb_grid
  )

# best hyperparameters

xgb_ajuste$bestTune

# tuning

tune_grid <- expand.grid(
  nrounds = 200,  
  max_depth = 3,
  colsample_bytree = 1,
  eta = 0.01,
  gamma = 0,
  min_child_weight = 1,
  subsample = 0.5
)

# tuning with best hyperparameters

xgb_modelo <- train(
  X_treino, y_treino,
  method = "xgbTree",
  trControl = xgb_control,
  tuneGrid = tune_grid
  )

# Performance model ---------------

# classification rule

class_xgb <- predict(
  xgb_modelo, 
  newdata = X_teste
)

# measures

medidas_xgb <- medidas_desempenho(
  dados_teste$insulina, 
  class_xgb); medidas_xgb



