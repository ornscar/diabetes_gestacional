# Individual Conditional Expectation (ICE) Plot on the XGBoost model

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

# Interpretability ----------------------

ice <- FeatureEffect$new(
  predictor, 
  method = "pdp+ice", 
  feature = "glicemia_jejum"
)

levels(ice$results$.class) <- c("sim","não")

ice$results <- filter(ice$results, as.integer(.class) == 1)

g <- plot(ice) + 
  ggtitle("ICE") + 
  scale_y_continuous("predicted insulin probability") +
  theme_bw()

g

saveRDS(g, "g_ice.rds")

ggsave(
  "result/ice_plot_xgb.png",
  width = 16,
  height = 10
)
