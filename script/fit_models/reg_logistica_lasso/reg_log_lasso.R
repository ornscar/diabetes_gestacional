# Lasso Logistic Regression

# Libraries ----------------------------

library(dplyr)
library(magrittr)
library(janitor)
library(glmnet)
library(caret)
library(cutpointr)

# Loading data -------------------------

dados_modelo <- readr::read_delim(
  "data/data_model.csv", 
  ";", 
  escape_double = FALSE, 
  trim_ws = TRUE
)

# Tidying data -------------------------

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

# Scaled data --------------------------

dados_selecionados <- dados_sem_na %>%
  dplyr::select(idade, n_gestacoes, glicemia_jejum) %>% 
  mutate_if(is.numeric, scale) %>% 
  mutate(imc_classe = dados_sem_na$imc_classe,
         familia_diabetes = dados_sem_na$familia_diabetes,
         ant_macrossomia = dados_sem_na$ant_macrossomia,
         pessoal_de_dmg = dados_sem_na$pessoal_de_dmg,
         tabagismo = dados_sem_na$tabagismo,
         hipertensao = dados_sem_na$hipertensao,
         insulina = dados_sem_na$insulina) %>% 
  as_tibble()

# Train/test data ----------------------

set.seed(1234)

particionando_dados <- createDataPartition(
  y = dados_selecionados$insulina, 
  p = 0.75, 
  list = FALSE
) 

dados_treino <- dados_selecionados[particionando_dados, ]
X_tr <- dados_treino[ , 1:9] %>% 
  as.matrix()
y_tr <- dados_treino$insulina

dados_teste <- dados_selecionados[-particionando_dados, ]

# Cross-validation ----------------------

k <- 10 

set.seed(1234)

cruzada_val <- createFolds(
  y = dados_treino$insulina, 
  k = k, 
  list = TRUE, 
  returnTrain = FALSE
)

# Fit model ------------------------------

ajuste_lasso_cv <- cv.glmnet(
  X_tr, 
  y_tr, 
  alpha = 1, 
  family = "binomial"
)

lambda <- ajuste_lasso_cv$lambda.min

#coef(ajuste_lasso_cv, s = "lambda.min")

# function to find threshold

predito_lasso <- NULL
verd_lasso <- NULL

for (j in 1:k){
  X_treino <- dados_treino[-cruzada_val[[j]], 1:9] %>% as.matrix()
  y_treino <- dados_treino$insulina[-cruzada_val[[j]]]  
  aj_lasso <- glmnet(X_treino, y_treino, alpha = 1, family = "binomial")
  p_est <-  predict(aj_lasso, newx = dados_treino[cruzada_val[[j]], 1:9] %>% 
                      as.matrix(), s = lambda, type = "response")
  predito_lasso <- c(predito_lasso, p_est)
  verd_lasso <- c(verd_lasso, dados_treino[cruzada_val[[j]], ]$insulina)
}

# probabilities

df_lasso <- data.frame(
  y =  verd_lasso, 
  prob = predito_lasso
)

# measures

cp_lasso <- cutpointr(
  x = df_lasso$prob, 
  class = df_lasso$y, 
  method = minimize_metric, 
  metric = roc01
)

#summary(cp_lasso)
#plot(cp_lasso)

# threshold
pc_lasso <- cp_lasso$optimal_cutpoint
# auc
auc_lasso <- cp_lasso$AUC
# accuracy
acuracia_treina_lasso <- cp_lasso$acc

# predictions

previsoes_lasso <- predict(
  ajuste_lasso_cv, 
  newx = dados_teste[, 1:9] %>% 
    as.matrix(), s = "lambda.min", type = "response"
)

# Performance model ------------------------

# classification rule

class_lasso <- ifelse(previsoes_lasso < pc_lasso, 0, 1)

# measures

medidas_lasso <- medidas_desempenho(
  dados_teste$insulina, class_lasso
); medidas_lasso
