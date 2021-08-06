library(mrpkit)
library(brms)

map_model <- readRDS("results/map_model.rds")

fit2 <- map_model$fit(
  fun = brms::brm,
  formula = vote_post ~ (1|age) + (1|gender) + (1|state) + (1|collapsed_re) + (1|education),
  family = "bernoulli",
  refresh = 100,
  cores = 2,
  backend = "cmdstanr"
)

# save the model 
saveRDS(fit2, "fit2.rds")