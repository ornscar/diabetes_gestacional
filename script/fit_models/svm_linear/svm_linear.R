# Linear Support Vector Machines

# Libraries ----------------------------

library(dplyr)
library(magrittr)
library(janitor)
library(caret)
library(e1071)
library(cutpointr)

# Loading data -------------------------

dados_modelo <- readr::read_delim(
  "data/data_model.csv", 
  ";", 
  escape_double = FALSE, 
  trim_ws = TRUE
)

# Tidying data --------------------------

dados_modelo <- dados_modelo %>% 
  clean_names()

dados_modelo <- dplyr::rename(
  dados_modelo, 
  n_gestacoes = g, 
  hipertensao = hac,
  glicemia_jejum = glicema_jejum
)

dados_modelo <- dados_modelo %>% 
  mutate(ano = gsub("[.]","", ano))

dados_modelo$imc_classe <- factor(
  dados_modelo$imc_classe, 
  labels = c("até normal",
             "sobrepeso", 
             "obeso")
)

dados_modelo$familia_diabetes <- factor(
  dados_modelo$familia_diabetes, 
  labels = c("não", 
             "sim")
)

dados_modelo$ant_macrossomia <- factor(
  dados_modelo$ant_macrossomia, 
  labels = c("não", 
             "sim")
)

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

dados_modelo$tabagismo <- factor(
  dados_modelo$tabagismo, 
  labels = c("não", 
             "sim")
)

dados_modelo$hipertensao <- factor(
  dados_modelo$hipertensao, 
  levels = c("0", "1"),
  labels = c("não", "sim")
)

# Selected data -------------------------

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

# sem NA

dados_sem_na <- vas_selecionadas[complete.cases(vas_selecionadas) == TRUE, ]

# Scaled data ----------------------------

dados_selecionados <- dados_sem_na %>%
  dplyr::select(-insulina) %>% 
  mutate_if(is.numeric, scale) %>% 
  mutate(insulina = dados_sem_na$insulina) %>% 
  as_tibble()

# Train/test data -------------------------

set.seed(1234)

particionando_dados <- createDataPartition(
  y = dados_selecionados$insulina, 
  p = 0.75, 
  list = FALSE
) 

dados_treino <- dados_selecionados[particionando_dados, ]

dados_teste <- dados_selecionados[-particionando_dados, ]

# Cross-validation -------------------------

k <- 10 

set.seed(1234)

cruzada_val <- createFolds(
  y = dados_treino$insulina, 
  k = k, 
  list = TRUE, 
  returnTrain = FALSE
)

# Fit model ---------------------------------

# tuning parameters

escolha_m <- tune.svm(
  insulina ~ .,
  data = dados_treino, 
  cost = seq(0.001, 5, by = 0.5)
)

svm.model <- svm(
  insulina ~ .,
  data = dados_treino, 
  kernel = "linear", 
  cost = as.numeric(escolha_m$best.parameters[1])
)

#summary(svm.model)

# test sample predictions

pred_aux <- predict(
  svm.model, 
  newdata = dados_teste, 
  decision.values = TRUE, 
  probability = TRUE
)

pred <- attr(
  pred_aux, 
  "decision.values"
)

# Performance model -------------------------

# classification rule

predito_svm_linear <- ifelse(pred < 0, 0, 1)

# confusion matrix

conf_matrix <- table(
  predict = predito_svm_linear, 
  true = dados_teste$insulina
)

# measures

medidas_svm_lin <- medidas_desempenho(
  teste = dados_teste$insulina, 
  classi = predito_svm_linear
); medidas_svm_lin
