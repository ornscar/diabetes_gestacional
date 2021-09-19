# Shapley Values Plot on the XGBoost model

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

shapley <- Shapley$new(
  predictor, 
  x.interest = X[10, ]
)

shapley$results <- filter(shapley$results, as.integer(class) == 1)

g <- shapley$plot() + 
  ggtitle("Shapley Values") +
  labs(y = "Shapley value", x = "feature values") +
  theme_bw()

g

saveRDS(g, "g_shapley.rds")

ggsave(
  "result/shapley_plot_xgb.png",
  width = 10,
  height = 10
)
