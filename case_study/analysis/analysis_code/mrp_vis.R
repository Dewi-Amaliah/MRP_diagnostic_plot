# All of visualisation codes used in Chapter 3 goes here

## ---- pkgs-chap3
library(mrpkit)
library(brms)
library(tidyverse)
library(tictoc)
library(ggplot2)
library(urbnmapr)
library(ggthemes)
library(kableExtra)
library(patchwork)
library(Metrics)
library(survey)
library(ggstance)
library(data.table)
library(ggpmisc)
library(scales)
library(wacolors)
library(tidytext)

options(scipen = 0.9999)

## ---- read-map-fits

# read map object
map_model<- readRDS(here::here("case_study/results/map_model.rds"))

# read the fit object
fit1 <- readRDS(here::here("case_study/results/fit1.rds"))
fit2 <- readRDS(here::here("case_study/results/fit2.rds"))
fit3 <- readRDS(here::here("case_study/results/fit3.rds"))
fit4a <- readRDS(here::here("case_study/results/fit4a.rds"))
fit4b <- readRDS(here::here("case_study/results/fit4b.rds"))

# predict population
fit1_predict <- fit1$population_predict()
fit2_predict <- fit2$population_predict()
fit3_predict <- fit3$population_predict()
fit4a_predict <- fit4a$population_predict()
fit4b_predict <- fit4b$population_predict()

# function to get estimation by state
get_state_pred <- function(fit_object, pop_predict){
  temp <- fit_object$aggregate(pop_predict, by = "state")
  temp %>%
    group_by(state) %>%
    summarise(median = median(value),
              lower = quantile(value, 0.025),
              upper = quantile(value, 0.975)) 
}

# estimation by state fit1 
state_est_base <- get_state_pred(fit1, fit1_predict) %>%
  rename(base_model = median,
         lower_base_model = lower,
         upper_base_model = upper)

# estimation by state fit3
state_est_modelb <- get_state_pred(fit2, fit2_predict) %>%
  rename(modelb = median,
         lower_modelb = lower,
         upper_modelb = upper)

# estimation by state fit3
state_est_modelc <- get_state_pred(fit3, fit3_predict) %>%
  rename(modelc = median,
         lower_modelc = lower,
         upper_modelc = upper)

# estimation by state fit4a
state_est_intent <- get_state_pred(fit4a, fit4a_predict) %>%
  rename(intent = median,
         lower_intent = lower,
         upper_intent = upper)

# estimation by state fit4b
state_est_party <- get_state_pred(fit4b, fit4b_predict) %>%
  rename(party = median,
         lower_party = lower,
         upper_party = upper)

# ground truth data 
ground_truth <- ddi::g2016 %>%
  select(c(1, 3)) %>%
  rename(truth = pct_djt_voters)

comparison_data <- plyr::join_all(list(ground_truth,
                                       state_est_base,
                                       state_est_modelb,
                                       state_est_modelc,
                                       state_est_intent,
                                       state_est_party), 
                                  by='state', 
                                  type='left')

## ---- choro
state_estimation_data <- left_join(urbnmapr::states, state_est_base, 
                                   by = c("state_name" = "state"))

ggplot(state_estimation_data, 
       aes(long, lat, group = group, fill = base_model)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn(colors=c("blue","white","red"),
                       values=rescale(c(0,0.5,1)),
                       limits=c(0,1)) +
  ggtitle("MPR estimates of Trump vote shares using the baseline model") +
  theme_map() +
  labs(fill = "Estimates of Trump's vote share") +
  theme(legend.position = "bottom")

## ---- est-comparison

# filter the na in vote variable
cces <- read_rds(here::here("case_study/data/cces_use.rds"))
cces_mrp <- cces %>%
  # rename variable as mrpkit workflow
  rename(wt = weight) %>%
  # mutate the outcome which is vote for Donald Trump (yes/no) in post vote
  # mutate the outcome which is vote for Donald Trump (yes/no) in early vote
  # mutate the outcome which is identified as Republican (yes) or no
  mutate( 
    vote = as.factor(ifelse(voted_pres_16 == "Donald Trump", "yes", "no")),
    intent = as.factor(ifelse(intent_pres_16 == "Donald Trump (Republican)", "yes", "no")),
    party = as.factor(ifelse(pid3_leaner == "Republican (Including Leaners)", "Republican", "not Republican"))
  )

cces_mrp_na_omit <- cces_mrp %>%
  filter(!is.na(vote))

# raw-est
raw_est <- cces_mrp_na_omit %>%
  mutate(vote_bin = ifelse(vote == "yes", 1, 0)) %>%
  group_by(state) %>%
  summarise(mean = mean(vote_bin),
            sd = sqrt(mean(vote_bin)*(1-mean(vote_bin))/length(vote_bin)),
            lower = mean - (1.96*sd/sqrt(length(vote_bin))),
            upper = mean + (1.96*sd/sqrt(length(vote_bin)))) %>%
  select(-sd) %>%
  mutate(est_method = "raw",
         state = as.character(state)) %>%
  arrange(state)

# wtd-est

# create survey object
wtd_est <- svydesign(ids = ~1, weights = ~wt, data = cces_mrp_na_omit)

# calculate wtd estimate who vote for Trump
wtd_mean <- data.frame(svyby(~vote, ~state, wtd_est, svymean)) %>%
  select(voteyes) %>%
  rename(wtd_est = voteyes) %>%
  rownames_to_column("state")
rownames(wtd_mean) <- 1:nrow(wtd_mean)

# calculate confidence interval for voter who vote for Trump
wtd_confint <- data.frame(confint(svyby(~vote, ~state, wtd_est, svymean))) %>%
  rename(lower_wt = X2.5..,
         upper_wt = X97.5..) %>%
  rownames_to_column("state") %>%
  filter(str_detect(state, "voteyes")) %>%
  separate(state, c("state", "vote"), ":") %>%
  select(-vote)

# join mean and confint
wtd_estimates <- left_join(wtd_mean, wtd_confint, by = "state") %>%
  rename(mean = wtd_est,
         lower = lower_wt,
         upper = upper_wt) %>%
  mutate(est_method = "wtd",
         state = as.character(state)) %>%
  arrange(state)

# mrp-est
mrp_estimates <- state_est_modelb %>%
  rename(mean = modelb,
         lower = lower_modelb,
         upper = upper_modelb) %>%
  mutate(est_method = "mrp")

compare_method <- left_join(raw_est, wtd_estimates, by = "state", suffix = c("_raw", "_wtd")) %>%
  left_join(mrp_estimates, by="state") %>%
  rename(mean_mrp = mean,
         lower_mrp = lower,
         upper_mrp = upper) %>%
  select(-est_method_raw, -est_method_wtd, est_method)

# truth 
truth <- comparison_data %>%
  select(state, truth)

compare_method_df <-  rbind(raw_est, wtd_estimates, mrp_estimates) %>%
  mutate(est_method = factor(est_method, levels = c("raw", "wtd", "mrp"))) %>%
  left_join(truth, by = "state") %>%
  mutate(est_method = factor(est_method, levels = c("truth", "raw", "wtd", "mrp")),
         deviance = mean - truth,
         lower_dev = lower - truth,
         upper_dev = upper - truth)

# RMSE of each model
rmse_raw <- round(rmse(truth$truth, raw_est$mean),3)
rmse_mrp <- round(rmse(truth$truth, comparison_data$modelb),3)
rmse_wtd <- round(rmse(truth$truth, wtd_estimates$mean),3)


# MAD of each model
mad_raw <- round(mad(truth$truth, raw_est$mean),3)
mad_mrp <- round(mad(truth$truth, comparison_data$modelb),3)
mad_wtd <- round(mad(truth$truth, wtd_estimates$mean),3)


metrics_method <- tribble(~"Method", ~ "RMSE", ~ "MAD",
                          "Raw", rmse_raw, mad_raw,
                          "Weighted", rmse_wtd, mad_wtd,
                          "MRP", rmse_mrp, mad_mrp)


## ---- scatter-est-method
ggplot(compare_method_df, 
       aes(x = truth, 
           y = mean, 
           ymin = lower, 
           ymax = upper,
           color = est_method)) +
  geom_point(size = 0.8, alpha = 0.9) +
  geom_abline(intercept = 0, linetype = "dotted") +
  geom_errorbar(size = 0.3,
                alpha = 0.3) +
  annotate(geom = "table",
           x = 0.99,
           y = 0,
           label = list(metrics_method),
           size = 2.5) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 8),
        legend.position = "bottom") +
  ylab("MRP estimates (Trump vote)") +
  xlab("Ground truth (Trump vote)") +
  theme(aspect.ratio=1) +
  scale_color_wa_d(palette = "rainier",
                   name = "Estimation methods", labels = c("Raw", "Weighted", "MRP"))

## ---- dot-est-method
compare_method_df %>%
  mutate(state = as.factor(state),
         state = fct_reorder(state, truth)) %>%
  ggplot(aes(x = deviance, y = state, color = est_method, xmin = lower_dev, xmax = upper_dev)) +
  geom_vline(xintercept = 0) +
  geom_point(position = position_dodgev(height = .5)) +
  geom_errorbarh(position = position_dodgev(height = .5),
                 alpha = 0.5) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 8)) +
  xlab("Deviance from the actual value of probability vote for Trump (Estimates - Actual)") +
  ylab("State") +
  scale_color_wa_d(palette = "rainier",
                   name = "Estimation methods", labels = c("Raw", "Weighted", "MRP"))

## ---- met-fun
# function to get RMSE
get_rmse <- function(vect){
  paste0("RMSE= ", round(rmse(comparison_data$truth, vect), 4))
}

# function to get MAD
get_mad <- function(vect){
  paste0("MAE= ", round(mae(comparison_data$truth, vect), 4))
}

# RMSE of each model
rmse_base <- get_rmse(comparison_data$base_model)
rmse_modelb <- get_rmse(comparison_data$modelb)
rmse_modelc <- get_rmse(comparison_data$modelc)
rmse_intent <- get_rmse(comparison_data$intent)
rmse_party <- get_rmse(comparison_data$party)

# MAD of each model
mad_base <- get_mad(comparison_data$base_model)
mad_modelb <- get_mad(comparison_data$modelb)
mad_modelc <- get_mad(comparison_data$modelc)
mad_intent <- get_mad(comparison_data$intent)
mad_party <- get_mad(comparison_data$party)


## ---- state-wise-scatter
get_scatter <- function(y, title, lower, upper, rmse, mad, ylab){
  ggplot(comparison_data, 
         aes(x = truth, 
             y = y, 
             ymin = lower, 
             ymax = upper)) +
    geom_point(color = "#DE0100", size = 1, alpha = 0.7) +
    geom_abline(intercept = 0, linetype = "dotted") +
    geom_errorbar(color = "#DE0100", size = 0.3) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_bw() +
    theme(axis.title = element_text(size = 8),
          plot.title = element_text(size = 10)) +
    ylab(ylab) +
    xlab("Ground truth (Trump vote)") +
    theme(aspect.ratio=1) +
    ggtitle(title) +
    annotate("text", x = 0.75, 
             y = 0.10, 
             label = rmse, 
             vjust = -1,
             size = 2) +
    annotate("text", x = 0.75, 
             y = 0.03, 
             label = mad, 
             vjust = -1,
             size = 2) 
}

sp1 <- get_scatter(comparison_data$base_model, 
                   "A",
                   comparison_data$lower_base_model,
                   comparison_data$upper_base_model,
                   rmse_base,
                   mad_base,
                   "MRP estimates(Trump vote)")

sp2 <-  get_scatter(comparison_data$modelb, 
                    "B",
                    comparison_data$lower_modelb,
                    comparison_data$upper_modelb,
                    rmse_modelb,
                    mad_modelb,
                    "MRP estimates(Trump vote)")

sp3 <- get_scatter(comparison_data$modelc, 
                   "C",
                   comparison_data$lower_modelc,
                   comparison_data$upper_modelc,
                   rmse_modelc,
                   mad_modelc,
                   "MRP estimates(Trump vote)")

sp4a <- get_scatter(comparison_data$intent, 
                    "D",
                    comparison_data$lower_intent,
                    comparison_data$upper_intent,
                    rmse_intent,
                    mad_intent,
                    "MRP Estimates (Trump vote intention)")

sp4b <- get_scatter(comparison_data$party, 
                    "E",
                    comparison_data$lower_party,
                    comparison_data$upper_party,
                    rmse_party,
                    mad_party,
                    "MRP Estimates (Republican)")

sp1 + sp2 + sp3 + sp4a + sp4b + plot_layout(nrow = 3)

## ---- violin-data

# estimate probability of voting Trump by race 
race_est_base <- fit1$aggregate(fit1_predict, by = "original_re") %>%
  mutate(model = "base model")
race_est_mod3 <- fit3$aggregate(fit3_predict, by = "original_re") %>%
  mutate(model = "base model with more levels on race")
comp_race <- rbind(race_est_base, race_est_mod3) 


# estimate prob of voting Trump by education
educ_est_base <- fit1$aggregate(fit1_predict, by = "education") %>%
  mutate(model = "base model")
educ_est_mod2 <- fit2$aggregate(fit2_predict, by = "education") %>%
  mutate(model = "base model + education")
comp_educ <- rbind(educ_est_base, educ_est_mod2) 

# estimate prob of voting Trump by age
age_est_base <- fit1$aggregate(fit1_predict, by = "age") %>%
  mutate(model = "base model")
age_est_mod2 <- fit2$aggregate(fit2_predict, by = "age") %>%
  mutate(model = "base model + education")
age_est_mod3 <- fit3$aggregate(fit3_predict, by = "age") %>%
  mutate(model = "base model with more race categories")
comp_age <- rbind(age_est_base, age_est_mod2, age_est_mod3) %>%
  mutate(model = factor(model, levels = c("base model",
                                          "base model + education", 
                                          "base model with more race categories")))

## ---- violin-levels-plot

p6 <- ggplot(comp_race, aes(x = original_re, y = value, fill = model)) +
  geom_violin() +
  xlab("Race") +
  ylab("MRP estimates") +
  theme_bw() +
  theme(axis.title = element_text(size = 8),
        plot.title = element_text(size = 12),
        axis.text.x = element_text(angle = 20, size = 7),
        legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle("A") +
  scale_fill_manual(values = c("#616A6B", "#d01c8b")) 

# create the plot for education 
p7 <- ggplot(comp_educ, aes(x = education, y = value, fill = model)) +
  geom_violin() +
  xlab("Education") +
  ylab("MRP estimates") +
  theme_bw() +
  theme(axis.title = element_text(size = 8),
        plot.title = element_text(size = 12),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 7),
        legend.position = "bottom") +
  ggtitle("B") +
  scale_fill_manual(values = c("#616A6B", "#E67E22")) 

# create the plot for age
p8 <- ggplot(comp_age, aes(x = age, y = value, fill = model)) +
  geom_violin() +
  xlab("Age") +
  ylab("MRP estimates") +
  theme_bw() +
  theme(axis.title = element_text(size = 8),
        plot.title = element_text(size = 12),
        axis.text.x = element_text(angle = 20, size = 7),
        legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle("C") +
  scale_fill_manual(values = c("#616A6B", "#E67E22", "#d01c8b")) 

p6 + p7 + p8 + plot_layout(nrow = 3)

## ---- perf-metrics

# estimate poststrat function 
get_ests_ps <- function(model){
  ests_ps <- cbind(map_model$poststrat_data(), model)
  return(ests_ps)
}

# MRP estimate by state and other variable

get_MRP_ests <- function(est_data, var){
  tmp <- est_data %>%
    pivot_longer(as.character(1:4000),
                 names_to = "posterior_draw",
                 values_to = "posterior_sample") %>%
    group_by(state, {{var}}, posterior_draw) %>% # Add extra grouping vars here
    summarise(post_strat_ests = sum(posterior_sample*N_j)/sum(N_j))
  
  return_df <- tmp %>%
    group_by(state, {{var}}) %>%
    summarise(median = median(post_strat_ests),
              lower = quantile(post_strat_ests, 0.025),
              upper = quantile(post_strat_ests, 0.975),
              ci_len = upper - lower)
  
  return(return_df)
}

# estimate poststrat by model 
ests_ps_fit1 <- get_ests_ps(fit1_predict)
ests_ps_fit2 <- get_ests_ps(fit2_predict)
ests_ps_fit3 <- get_ests_ps(fit3_predict)

# MRP estimate by race 
mrp_race_state_fit1 <- get_MRP_ests(ests_ps_fit1, original_re)
mrp_race_state_fit3 <- get_MRP_ests(ests_ps_fit3, original_re)

# MRP estimate by education
mrp_educ_state_fit1 <- get_MRP_ests(ests_ps_fit1, education)
mrp_educ_state_fit2 <- get_MRP_ests(ests_ps_fit2, education)

# MRP estimate by age 
mrp_age_state_fit1 <- get_MRP_ests(ests_ps_fit1, age)
mrp_age_state_fit2 <- get_MRP_ests(ests_ps_fit2, age)
mrp_age_state_fit3 <- get_MRP_ests(ests_ps_fit3, age)

# in total 
get_metrics <- function(vect, upper, lower){
  cor_adj <- 1 - cor(comparison_data$base_model, vect)
  mae <- mae(comparison_data$base_model, vect)
  rmse <- rmse(comparison_data$base_model, vect)
  
  metrics <- tribble(~level, ~metrics, ~values,
                     "Statewide", "rmse", rmse,
                     "Statewide", "cor_adj", cor_adj,
                     "Statewide", "mae", mae) %>%
    mutate(level = as.factor(level),
           metrics = as.factor(metrics))
  
  return(metrics)
}

metrics_fit1_fit3 <- get_metrics(comparison_data$modelc, 
                                 comparison_data$upper_modelc,
                                 comparison_data$lower_modelc) 

# by level
race_metrics_tmp <- left_join(mrp_race_state_fit1, mrp_race_state_fit3, by = c("state", "original_re"), 
                              suffix = c("_fit1", "_fit3")) %>%
  group_by(original_re) %>%
  summarise(cor_adj = 1 - cor(median_fit1, median_fit3),
            mae = mae(median_fit1, median_fit3),
            rmse = rmse(median_fit1, median_fit3)) %>%
  pivot_longer(c(2:4), names_to = "metrics", values_to = "values") %>%
  rename(level = original_re)

race_metrics <- rbind(race_metrics_tmp, metrics_fit1_fit3) %>%
  mutate(metrics = factor(metrics, levels = c("cor_adj", "rmse", "mae", "ci_len")),
         level = factor(level, levels = c("Black", "White", "Hispanic", "Asian", "Native American",
                                          "All Other", "Statewide")))

## ---- metrics-plot-fun

# function to get plot
metrics_label <- c(`cor_adj` = "1 - correlation",
                   `rmse` = "Root Mean Square Difference",
                   `mae` = "Mean Absolute Difference")

get_metrics_plot <- function(data, title, subtitle, xlab){
  plot <- data %>%
    mutate(level = reorder(level, values)) %>%
    group_by(metrics, level) %>%
    arrange(desc(values)) %>%
    ungroup() %>%
    mutate(level = factor(paste(level, metrics, sep = "__"), 
                          levels = rev(paste(level, metrics, sep = "__")))) %>%
    ggplot(aes(level, values)) +
    geom_segment(aes(xend = level, yend = 0), 
                 colour = "grey50") +
    geom_point(size = 2) + 
    ggtitle(title,
            subtitle = subtitle) +
    facet_wrap(~metrics, scales = "free", ncol = 1,
               labeller = as_labeller(metrics_label)) +
    scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
    scale_y_continuous(labels = label_comma()) +
    coord_flip() +
    scale_colour_brewer(palette = "Dark2") +
    theme_bw() +
    xlab(xlab) +
    ylab("Values") +
    theme(panel.grid.major.y = element_blank()) + 
    theme(legend.position = "none",
          axis.ticks.y = element_blank(),
          axis.title = element_text(size = 9),
          axis.text.x = element_text(size = 6),
          plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 8))
  return(plot)
}


## ---- ci-len-data

# function to get the plot

ci_label <- c(`mean_ratio` = "CI length ratio",
              `mean_ci_dev` = "CI length difference")

get_ci_plot <- function(data, level, title, subtitle, xlab){
  plot <- data %>%
    mutate(level = reorder(level, values)) %>%
    group_by(metrics, level) %>%
    arrange(desc(values)) %>%
    ungroup() %>%
    mutate(level = factor(paste(level, metrics, sep = "__"), 
                          levels = rev(paste(level, metrics, sep = "__")))) %>%
    ggplot(aes(level, values)) +
    geom_segment(aes(xend = level, yend = 0)) +
    geom_point(size = 2) + 
    ggtitle(title,
            subtitle = subtitle) +
    facet_wrap(~metrics, scales = "free", ncol = 2,
               labeller = as_labeller(ci_label)) +
    scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
    coord_flip() +
    scale_colour_brewer(palette = "Dark2") +
    theme_bw() +
    xlab(xlab) +
    ylab("Values") +
    theme(panel.grid.major.y = element_blank()) + 
    theme(legend.position = "none",
          axis.title = element_text(size = 8),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 10, size = 7),
          plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 8)) 
  return(plot)
}


## ---- cor-race-plot

get_metrics_plot(race_metrics, 
                 "Metrics by race categories",
                 "Model with more race categories compared to the baseline model",
                 "Race categories")


## ---- race-ci-len

compare_ci_len_race <- left_join(mrp_race_state_fit1, mrp_race_state_fit3, by = c("state", "original_re"), 
                                 suffix = c("_fit1", "_fit3")) %>%
  mutate(ci_len_ratio = ci_len_fit3/ci_len_fit1,
         ci_dev = ci_len_fit3 - ci_len_fit1) %>%
  group_by(original_re) %>%
  summarise(mean_ratio = mean(ci_len_ratio),
            mean_ci_dev = mean(ci_dev)) %>%
  pivot_longer(c(2:3), names_to = "metrics", values_to = "values")

get_ci_plot(compare_ci_len_race, compare_ci_len_race$original_re, "Comparison of credible interval", 
            "Model with more race categories compared to the baseline model",
            "Race categories")


## ---- cor-edu-plot

get_metrics_plot(educ_metrics,
                 "Metrics by education level",
                 "Model with additional predictor (education) compared to the base model",
                 "Education levels")

## ---- edu-ci-len

compare_ci_len_ed <- left_join(mrp_educ_state_fit1, mrp_educ_state_fit2, by = c("state", "education"),
                               suffix = c("_fit1", "_fit2")) %>%
  mutate(ci_len_ratio = ci_len_fit2/ci_len_fit1,
         ci_dev = ci_len_fit2 - ci_len_fit1) %>%
  group_by(education) %>%
  summarise(mean_ratio = mean(ci_len_ratio),
            mean_ci_dev = mean(ci_dev)) %>%
  pivot_longer(c(2:3), names_to = "metrics", values_to = "values")

get_ci_plot(compare_ci_len_ed, compare_ci_len_ed$education, "Comparison of credible interval", 
            "Model with education level compared to base model",
            "Education level")


## ---- apd-educ-metrics

metrics_fit1_fit2 <- get_metrics(comparison_data$modelb,
                                 comparison_data$upper_modelb,
                                 comparison_data$lower_modelb)

educ_metrics_tmp <- left_join(mrp_educ_state_fit1, mrp_educ_state_fit2, by = c("state", "education"), 
                              suffix = c("_fit1", "_fit2")) %>%
  group_by(education) %>%
  summarise(cor_adj = 1 - cor(median_fit1, median_fit2),
            mae = mae(median_fit1, median_fit2),
            rmse = rmse(median_fit1, median_fit2)) %>%
  pivot_longer(c(2:4), names_to = "metrics", values_to = "values") %>%
  rename(level = education)

educ_metrics <- rbind(educ_metrics_tmp, metrics_fit1_fit2) %>%
  mutate(metrics = factor(metrics, levels = c("cor_adj", "rmse", "mae")))

get_metrics_plot(educ_metrics,
                 "Estimation performance criteria by education level",
                 "Model with additional predictor (education) compared to base model",
                 "Education levels")

## ---- apd-ci-len-edu

compare_ci_len_ed <- left_join(mrp_educ_state_fit1, mrp_educ_state_fit2, by = c("state", "education"),
                               suffix = c("_fit1", "_fit2")) %>%
  mutate(ci_len_ratio = ci_len_fit2/ci_len_fit1,
         ci_dev = ci_len_fit2 - ci_len_fit1) %>%
  group_by(education) %>%
  summarise(mean_ratio = mean(ci_len_ratio),
            mean_ci_dev = mean(ci_dev)) %>%
  pivot_longer(c(2:3), names_to = "metrics", values_to = "values")

get_ci_plot(compare_ci_len_ed, compare_ci_len_ed$education, "Comparison of credible interval", 
            "Model with education level compared to base model",
            "Education level")


## ---- apd-age-fit2

age_metrics_tmp <- left_join(mrp_age_state_fit1, mrp_age_state_fit2, by = c("state", "age"),
                             suffix = c("_fit1", "_fit2")) %>%
  group_by(age) %>%
  summarise(cor_adj = 1 - cor(median_fit1, median_fit2),
            mae = mae(median_fit1, median_fit2),
            rmse = rmse(median_fit1, median_fit2)) %>%
  pivot_longer(c(2:4), names_to = "metrics", values_to = "values") %>%
  rename(level = age)

age_metrics <- rbind(age_metrics_tmp, metrics_fit1_fit2) %>%
  mutate(metrics = factor(metrics, levels = c("cor_adj", "rmse", "mae", "ci_len")),
         level = factor(level, levels = c("18 to 24 years", "25 to 34 years",
                                          "35 to 44 years", "45 to 64 years",
                                          "65 years and over", "Statewide")))

get_metrics_plot(age_metrics,
                 "Metrics by age group",
                 "Model with additional predictor (education) compared to the base model",
                 "Age group")

## ---- apd-age-ci-fit2

compare_ci_len_age2 <- left_join(mrp_age_state_fit1, mrp_age_state_fit2, by = c("state", "age"),
                                 suffix = c("_fit1", "_fit2")) %>%
  mutate(ci_len_ratio = ci_len_fit2/ci_len_fit1,
         ci_dev = ci_len_fit2 - ci_len_fit1) %>%
  group_by(age) %>%
  summarise(mean_ratio = mean(ci_len_ratio),
            mean_ci_dev = mean(ci_dev)) %>%
  pivot_longer(c(2:3), names_to = "metrics", values_to = "values")

get_ci_plot(compare_ci_len_age2, compare_ci_len_age2$age, "Comparison of credible interval length", 
            "Model with education level as predictor compared to base model",
            "Age group")

## ---- apd-age-fit3

age_metrics2_tmp <- left_join(mrp_age_state_fit1, mrp_age_state_fit3, by = c("state", "age"),
                              suffix = c("_fit1", "_fit3")) %>%
  group_by(age) %>%
  summarise(cor_adj = 1 - cor(median_fit1, median_fit3),
            mae = mae(median_fit1, median_fit3),
            rmse = rmse(median_fit1, median_fit3)) %>%
  pivot_longer(c(2:4), names_to = "metrics", values_to = "values") %>%
  rename(level = age)

age_metrics2 <- rbind(age_metrics2_tmp, metrics_fit1_fit3) %>%
  mutate(metrics = factor(metrics, levels = c("cor_adj", "rmse", "mae", "ci_len")),
         level = factor(level, levels = c("18 to 24 years", "25 to 34 years",
                                          "35 to 44 years", "45 to 64 years",
                                          "65 years and over", "Statewide")))

get_metrics_plot(age_metrics2,
                 "Metrics by age level",
                 "Model with more race caegories compared to the base model",
                 "Age levels")

## ---- apd-age-ci-fit3

compare_ci_len_age3 <- left_join(mrp_age_state_fit1, mrp_age_state_fit3, by = c("state", "age"),
                                 suffix = c("_fit1", "_fit3")) %>%
  mutate(ci_len_ratio = ci_len_fit3/ci_len_fit1,
         ci_dev = ci_len_fit3 - ci_len_fit1) %>%
  group_by(age) %>%
  summarise(mean_ratio = mean(ci_len_ratio),
            mean_ci_dev = mean(ci_dev)) %>%
  pivot_longer(c(2:3), names_to = "metrics", values_to = "values")

get_ci_plot(compare_ci_len_age3, compare_ci_len_age3$age, "Comparison of credible interval length", 
            "Model with more race categories compared to base model",
            "Age group")

