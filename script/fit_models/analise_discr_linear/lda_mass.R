# Linear Discriminant Analysis

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

# Fit model without ROC ------------------

ajuste_lda <- lda(
  insulina ~ .,
  data = dados_treino
)

# test sample predictions

predicao_lda <- ajuste_lda %>% 
  predict(dados_teste)

# confusion matrix

conf_matrix <- table(
  predict = predicao_lda$class, 
  true = dados_teste$insulina
)

# measures

medidas_lda <- medidas_desempenho(
  teste = dados_teste$insulina, 
  classi = predicao_lda$class
); medidas_lda

# Fit model with ROC ---------------------

ajuste_lda_roc <- lda(
  x = X_tr, 
  grouping = y_tr
)

# find the best threshold
predito_lda <- NULL 
verd_lda <- NULL

for (j in 1:k){
  X_treino <- dados_num_treino[-cruzada_val[[j]], 1:9] %>% as.matrix()
  y_treino <- dados_num_treino$insulina[-cruzada_val[[j]]]
  aj_lda <- lda(x = X_treino, grouping = y_treino)
  p_aux <- predict(aj_lda, newdata = dados_num_treino[cruzada_val[[j]], 1:9] %>% as.matrix())
  p_est <- p_aux$posterior[ ,2] 
  predito_lda <- c(predito_lda, p_est)
  verd_lda <- c(verd_lda, dados_num_treino[cruzada_val[[j]], ]$insulina)
}

# probabilities

df_lda <- data.frame(
  y = verd_lda, 
  prob = predito_lda
)

# measures

cp_lda <- cutpointr(
  x = df_lda$prob, 
  class = df_lda$y, 
  method = minimize_metric, 
  metric = roc01
)

#summary(cp_lda)
#plot(cp_lda)

# threshold
pc_lda <- cp_lda$optimal_cutpoint
# auc
auc_lda <- cp_lda$AUC
# accuracy
acuracia_treina_lda <- cp_lda$acc

# test sample predictions

previsoes_lda <- predict(
  ajuste_lda_roc, 
  newdata = dados_num_teste[ , 1:9] %>% as.matrix()
)$posterior[ , 2]

# performance model 

# classification rule
class_lda_roc <- ifelse(previsoes_lda < pc_lda, 0, 1)

# measures
medidas_lda_roc <- medidas_desempenho(
  dados_num_teste$insulina, 
  class_lda_roc
); medidas_lda_roc
