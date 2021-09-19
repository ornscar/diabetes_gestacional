# Bagging

# Libraries ----------------------------

library(dplyr)
library(magrittr)
library(janitor)
library(caret)
library(cutpointr)

# Loading data ------------------------

dados_modelo <- readr::read_delim(
  "data/data_model.csv", 
  ";", 
  escape_double = FALSE, 
  trim_ws = TRUE
)

# Tidying data -------------------------

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

dados_modelo$insulina <- factor(
  dados_modelo$insulina, 
  levels = c(0, 1),
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

# Scaled data ---------------------------

dados_selecionados <- dados_sem_na %>%
  mutate_if(is.numeric, scale) %>% 
  as_tibble()

# Train/test data -----------------------

set.seed(1234)

particionando_dados <- createDataPartition(
  y = dados_selecionados$insulina, 
  p = 0.75, 
  list = FALSE
) 

dados_treino <- dados_selecionados[particionando_dados, ]

dados_teste <- dados_selecionados[-particionando_dados, ]

# Cross-validation -----------------------

control <- trainControl(
  method = 'repeatedcv', 
  number = 10, 
  repeats = 3, 
  search = 'grid'
)

# Fit model ------------------------------

ajuste_bag <- train(
  insulina ~ ., 
  data = dados_treino, 
  method = "treebag",
  metric = "Accuracy",
  trControl = control
)

# Performance model ----------------------

# classification rule

class_bag <- predict(
  ajuste_bag, 
  newdata = dados_teste
)

# measures

medidas_bag <- medidas_desempenho(
  dados_teste$insulina, 
  class_bag
); medidas_bag
