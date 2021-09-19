# Permutation Feature Importance on the XGBoost model

# Libraries ----------------------------

library(janitor)
library(ggthemes)
library(iml)

# Predictor ----------------------------

set.seed(310)

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

feat_imp <- FeatureImp$new(
  predictor, 
  loss = "ce"
)

g <- plot(feat_imp) + 
  ggtitle("Feature Importance") +
  scale_x_continuous("importance") +
  scale_y_discrete("feature") +
  theme_bw()

g

saveRDS(g, "g_featimp.rds")

ggsave(
  "result/feat-imp_plot_xgb.png",
  width = 10,
  height = 10
)
