# Logistic Regression

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

dados_modelo$insulina <- as.factor(
  dados_modelo$insulina
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
  #dplyr::select(-insulina) %>% 
  mutate_if(is.numeric, scale) %>% 
  #mutate(insulina = dados_sem_na$insulina)
  as_tibble()

# Training/test data --------------------

set.seed(1234)

particionando_dados <- createDataPartition(
  y = dados_selecionados$insulina, 
  p = 0.75, 
  list = FALSE
) 

dados_treino <- dados_selecionados[particionando_dados, ]

dados_teste <- dados_selecionados[-particionando_dados, ]

# Cross-validation -----------------------

k <- 10 

set.seed(1234)

cruzada_val <- createFolds(
  y = dados_treino$insulina, 
  k = k, 
  list = TRUE, 
  returnTrain = FALSE
)

# Fit model ------------------------------

ajuste_logis <- glm(
  insulina ~ .,
  data = dados_treino, 
  family = binomial
)

# find the threshold

predito_logis <- NULL  
verd_logis <- NULL

for (j in 1:k){
  dados_aj <- dados_treino[-cruzada_val[[j]], ]
  aj_logis <- glm(insulina ~ .,
                  data = dados_aj, 
                  family = binomial)
  p_est <- predict(aj_logis, newdata = dados_treino[cruzada_val[[j]], ], type = "response" )
  predito_logis <- c(predito_logis, p_est)
  verd_logis <- c(verd_logis, dados_treino[cruzada_val[[j]], ]$insulina)
}

# probabilities

df <- data.frame(
  y = verd_logis, 
  prob = predito_logis
)

# measures

cp <- cutpointr(
  x = df$prob, 
  class = df$y, 
  method = minimize_metric, 
  metric = roc01
)

#summary(cp)
#plot(cp)

# threshold
pc_logis <- cp$optimal_cutpoint 
# auc
auc_logis <- cp$AUC 
# accuracy
acuracia_treina <- cp$acc

# test sample predictions

previsoes <- predict(
  ajuste_logis, 
  newdata = dados_teste, 
  type = "response"
)

# Performance model ----------------------

# classification rule

class_logis <- ifelse(previsoes < pc_logis, 0, 1)

# measures

medidas_logis <- medidas_desempenho(
  teste = dados_teste$insulina, 
  classi = class_logis
); medidas_logis

