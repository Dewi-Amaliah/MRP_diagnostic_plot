library(mrpkit)
library(brms)

map_model <- readRDS("/mnt/lustre/projects/Mona0070/dama0007/mrp_vis/map_model.rds")

fit4a <- map_model$fit(
  fun = brms::brm,
  formula = intent ~ (1|age) + (1|gender) + (1|state) + (1|collapsed_re) + (1|education),
  family = "bernoulli",
  refresh = 100,
  cores = 2,
  backend = "cmdstanr"
)

# save the model
saveRDS(fit4a, "/mnt/lustre/projects/Mona0070/dama0007/mrp_vis_output/fit4a.rds")
