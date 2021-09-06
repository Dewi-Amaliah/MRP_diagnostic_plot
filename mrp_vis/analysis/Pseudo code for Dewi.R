# Code to do multiple "by" groups

#Uses the example code from Survey Map

#use MRP up to the point of obtaining poststrat_estimates (dim of n_poststrat by number of samples)

# Get poststrat matrix

ex_map$poststrat_data()

# Flip and bind the two together

ests_ps <- cbind(ex_map$poststrat_data(), poststrat_estimates)

# Use group by and across
library(tidyverse)
mrp_ests <- ests_ps %>%
  pivot_longer(as.character(1:1000),
               names_to = "posterior_draw",
               values_to = "posterior_sample")%>%
  group_by(age, posterior_draw)%>% # Add extra grouping vars here
  summarise(post_strat_ests = sum(posterior_sample*N_j)/sum(N_j))

# To get posterior summaries (median, etc, just group over the grouping vars but not the posterior draw like this)

mrp_ests %>%
  group_by(age)%>%
  summarise(median_ps = median(post_strat_ests))

# Verify with aggregate:

fit_1$aggregate(poststrat_estimates, by = "age") %>%
  group_by(age) %>%
  summarise(median_ps = median(value))
