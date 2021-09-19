# Global Surrogate Plot on the XGBoost model

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

global_surrogate <- TreeSurrogate$new(
  predictor, 
  maxdepth = 3
)

g <- plot(global_surrogate) + 
  scale_x_discrete("class") +
  ggtitle("Global Surrogate") +
  theme_bw()

g 

saveRDS(g, 
        "g_global.rds")

ggsave(
  "result/global_surr_plot_xgboost.png",
  width = 16,
  height = 10
)

# r-Squared

global_surrogate$r.squared
