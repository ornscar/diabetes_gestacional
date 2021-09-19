# Feature Interaction on the XGBoost model

# Libraries ----------------------------

library(janitor)
library(ggthemes)
library(iml)

# Predictor ----------------------------

set.seed(25)

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

feat_int <- Interaction$new(
  predictor
)

feat_int$results <- filter(feat_int$results, as.integer(.class) == 1)

g1 <- plot(feat_int) + 
  ggtitle("Feature Interaction") +
  scale_x_continuous("overall interaction strength") +
  scale_y_discrete("feature") +
  theme_bw()

g1

saveRDS(g1, "g1_featint.rds")

ggsave(
  "result/feat-int_plot_xgb.png",
  width = 16,
  height = 10
)

two_feat_int <- Interaction$new(
  predictor, 
  feature = "glicemia_jejum"
)

two_feat_int$results <- filter(two_feat_int$results, as.integer(.class) == 1)

g2 <- plot(two_feat_int) + 
  ggtitle("Feature Interaction") +
  scale_x_continuous("2-way interaction strength") +
  scale_y_discrete("features") +
  theme_bw()

g2

saveRDS(g2, "g2_featint.rds")

ggsave(
  "result/two_feat-int_plot_xgb.png",
  width = 16,
  height = 10
)
