# Accumulated Local Effects (ALE) Plot on the XGBoost model

# Libraries ----------------------------

library(janitor)
library(ggthemes)
library(iml)

# Predictor ----------------------------

# data

X <- data_train[-10]

y <- data_train$insulina

# fitted model 

# xgb_wf

# function to predict new data

predict_function <- function(model, newdata) {
  predict(model, new_data = newdata)$.pred_class
}

# predictor 

predictor <- Predictor$new(
  model = xgb_wf, 
  data = X,
  predict.function = predict_function,
  y = y, 
  type = "prob"
)

# Interpretability ---------------------

ale <- FeatureEffect$new(
  predictor, 
  method = "ale", 
  feature = "glicemia_jejum"
)

ale$results <- filter(ale$results, as.integer(.class) == 1)

g <- ale$plot() + 
  ggtitle("ALE") + 
  scale_y_continuous("difference to the average prediction") +
  theme_bw()

g

saveRDS(g, "g_ale.rds")

ggsave(
  "result/ale_plot_xgb.png",
  width = 16,
  height = 10
)
