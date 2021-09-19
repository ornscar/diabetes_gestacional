# Quadratic Discriminant Analysis

# Libraries ----------------------------

library(dplyr)
library(magrittr)
library(janitor)
library(caret)
library(MASS)
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

# Scaled data ---------------------------

dados_selecionados <- dados_sem_na %>%
  dplyr::select(-insulina) %>% 
  mutate_if(is.numeric, scale) %>% 
  mutate(insulina = dados_sem_na$insulina) %>% 
  as_tibble()

# Numeric data --------------------------

# for ROC curve

dados_num <- dados_selecionados %>%
  dplyr::select(-insulina) %>% 
  mutate_if(is.factor, as.numeric) %>% 
  mutate(insulina = dados_sem_na$insulina) %>% 
  as_tibble()

# Training/test data --------------------

set.seed(1234)

particionando_dados <- createDataPartition(
  y = dados_selecionados$insulina, 
  p = 0.75, 
  list = FALSE
) 

# factor data

dados_treino <- dados_selecionados[particionando_dados, ]

dados_teste <- dados_selecionados[-particionando_dados, ]

# numeric data

dados_num_treino <- dados_num[particionando_dados, ]

X_tr <- dados_num_treino[ , 1:9] %>% 
  as.matrix()
y_tr <- dados_num_treino$insulina

dados_num_teste <- dados_num[-particionando_dados, ]

# Cross-validation ----------------------

k <- 10 

set.seed(1234)

cruzada_val <- createFolds(
  y = dados_treino$insulina, 
  k = k, 
  list = TRUE, 
  returnTrain = FALSE
)

# Fit model without ROC -----------------

ajuste_qda <- qda(
  insulina ~ .,
  data = dados_treino
)

# test sample predictions

predicao_qda <- ajuste_qda %>% 
  predict(dados_teste)

# measures

medidas_qda <- medidas_desempenho(
  teste = dados_teste$insulina, 
  classi = predicao_qda$class
); medidas_qda

# Fit model with ROC ------------------

ajuste_qda_roc <- qda(
  x = X_tr, 
  grouping = y_tr
)

# find the best threshold

predito_qda <- NULL 
verd_qda <- NULL

for (j in 1:k){
  X_treino <- dados_num_treino[-cruzada_val[[j]], 1:9] %>% as.matrix()
  y_treino <- dados_num_treino$insulina[-cruzada_val[[j]]]
  aj_qda <- qda(x = X_treino, grouping = y_treino)
  p_aux <- predict(aj_qda, newdata = dados_num_treino[cruzada_val[[j]], 1:9] %>% as.matrix())
  p_est <- p_aux$posterior[ , 2] 
  predito_qda <- c(predito_qda, p_est)
  verd_qda <- c(verd_qda, dados_num_treino[cruzada_val[[j]], ]$insulina)
}

# probabilities

df_qda <- data.frame(
  y = verd_qda, 
  prob = predito_qda
)
# measures

cp_qda <- cutpointr(
  x = df_qda$prob, 
  class = df_qda$y,
  method = minimize_metric, 
  metric = roc01
)

#summary(cp_qda)
#plot(cp_qda)

# threshold
pc_qda <- cp_qda$optimal_cutpoint
# auc
auc_qda <- cp_qda$AUC
# accuracy
acuracia_treina_qda <- cp_qda$acc

# test sample predictions 

previsoes_qda <- predict(
  ajuste_qda_roc, 
  newdata = dados_num_teste[ , 1:9]
)$posterior[ , 2]

# performance model 

# classification rule

class_qda_roc <- ifelse(previsoes_qda < pc_qda, 0, 1)

# measures

medidas_qda_roc <- medidas_desempenho(
  dados_num_teste$insulina, 
  class_qda_roc
); medidas_qda_roc
