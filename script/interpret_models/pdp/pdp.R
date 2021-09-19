# Partial Dependence Plot (PDP) on the XGBoost model

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

# Interpretability --------------------

pdp <- FeatureEffect$new(
  predictor, 
  method = "pdp", 
  feature = "glicemia_jejum"
)

pdp$results <- filter(pdp$results, as.integer(.class) == 1)

g1 <- plot(pdp) + 
  ggtitle("PDP") + 
  scale_y_continuous("predicted insulin probability") + 
  theme_bw()

g1

saveRDS(g1, "g1_pdp.rds")

ggsave(
  "result/pdp_plot_xgb.png",
  width = 10,
  height = 10
)

two_pdp <- FeatureEffect$new(
  predictor, 
  method = "pdp",
  feature = c("idade", "glicemia_jejum")
)

two_pdp$results <- filter(two_pdp$results, as.integer(.class) == 1)

g2 <- two_pdp$plot() + 
  ggtitle("Heat PDP") + 
  theme_bw() + 
  scale_fill_viridis_c(option = "inferno")

g2

saveRDS(g2, "g2_pdp.rds")

ggsave(
  "result/two_pdp_plot_xgb.png",
  width = 10,
  height = 10
)
