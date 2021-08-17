library(mrpkit)
library(brms)

map_model <- readRDS("/mnt/lustre/projects/Mona0070/dama0007/mrp_vis/map_model.rds")

fit3 <- map_model$fit(
  fun = brms::brm,
  formula = vote ~ (1|age) + (1|gender) + (1|state) + (1|original_re),
  family = "bernoulli",
  refresh = 100,
  cores = 2,
  backend = "cmdstanr"
)

# save the model 
saveRDS(fit3, "/mnt/lustre/projects/Mona0070/dama0007/mrp_vis_output/fit3.rds")