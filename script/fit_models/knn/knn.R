# KNN

# Libraries ----------------------------

library(dplyr)
library(magrittr)
library(janitor)
library(caret)
library(class)
library(ggplot2)
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

# Scaled data ----------------------------

dados_selecionados <- dados_sem_na %>%
  dplyr::select(idade, n_gestacoes, glicemia_jejum) %>% 
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

# Train/test data -----------------------

set.seed(1234)

particionando_dados <- createDataPartition(
  y = dados_selecionados$insulina, 
  p = 0.75, 
  list = FALSE
) 

dados_treino <- dados_selecionados[particionando_dados, ]

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

# Fit model -----------------------------

k_grid <- 70 

# find the best k

perc_erro <- NULL

for (i in 1:k_grid){
  erro_knn <- 0
  for (j in 1:k){
    aj_knn <- knn(train = dados_treino[-cruzada_val[[j]], 1:9], 
                  test = dados_treino[cruzada_val[[j]], 1:9], 
                  cl = dados_treino$insulina[-cruzada_val[[j]]], k = i)
    erro_knn <- erro_knn + sum(dados_treino$insulina[cruzada_val[[j]]] != aj_knn)
  }
  perc_erro[i] <- erro_knn/dim(dados_treino)[1]
}

k_valores <- 1:k_grid

# error percentages for each k

erro_df <- data.frame(
  perc_erro, 
  k_valores
)

#graph

ggplot(erro_df, aes(x = k_valores, y = perc_erro)) + 
  geom_point() + 
  geom_line(lty = "dotted", color = 'red') +
  theme_bw()

# the best k

k_escolhido <- erro_df$k_valores[which.min(erro_df$perc_erro)]

aj_knn_teste <- knn(
  train = dados_treino[ , 1:9],  
  test = dados_teste[ , 1:9], 
  cl = dados_treino$insulina, 
  k = k_escolhido
)

# confusion matrix
conf_matrix <- table(
  predict = aj_knn_teste, 
  true = dados_teste$insulina
)


# measures

medidas_knn <- medidas_desempenho(
  teste = dados_teste$insulina, 
  classi = aj_knn_teste
); medidas_knn


