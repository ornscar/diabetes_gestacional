# Local Surrogate (LIME) Plot on the XGBoost model

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

# predictor 

predictor <- Predictor$new(
  model = xgb_wf, 
  data = X,
  y = y, 
  type = "prob"
)

# Interpretability ---------------------

lime <- LocalModel$new(
  predictor, 
  k = 4,
  x.interest = X[10, ]
)

lime$results$.class <- ifelse(lime$results$.class == ".pred_sim", "sim", "não")

lime$results <- filter(lime$results, .class == "sim")

g <- plot(lime) + 
  ggtitle("LIME") +
  labs(x = "feature values") + 
  theme_bw()

g

saveRDS(g, "g_lime.rds")

ggsave(
  "result/lime_plot_xgb.png",
  width = 10,
  height = 10
)

