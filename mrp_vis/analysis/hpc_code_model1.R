library(mrpkit)
library(brms)

map_model <- readRDS("results/map_model.rds")

# Model 1
fit1 <- map_model$fit(
  fun = brms::brm,
  formula = vote_post ~ (1|age) + (1|gender) + (1|state) + (1|collapsed_re),
  family = "bernoulli",
  refresh = 100,
  cores = 2,
  backend = "cmdstanr"
)


# save the model 
saveRDS(fit1, "fit1.rds")