# All of R code used in Chapter 3 until model fitting goes here

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

## ---- acs-cces-read
# read the data
cces <- read_rds(here::here("case_study/data/cces_use.rds"))
acs <- read_rds(here::here("case_study/data/acs_use_wrangled.rds"))

## ---- ct-data
# data for frequency table
get_ct <- function(x){
  cces %>% group_by({{x}}) %>%
    count() %>%
    mutate(percentage = round(n/nrow(cces)*100,2)) %>%
    select(-n)
}

voted_ct <- get_ct(voted_pres_16) %>%
  rename(`Candidate voted` = voted_pres_16)
intent_ct <- get_ct(intent_pres_16) %>%
  rename(`Candidate will be voted` = intent_pres_16)
party_id_ct <- get_ct(pid3_leaner) %>%
  rename(`Party identity including leaners` = pid3_leaner)
gender_ct <- get_ct(gender) %>%
  rename(Gender = gender)
race_ct <- get_ct(race) %>%
  rename(Race = race)
age_ct <- get_ct(age) %>%
  rename(Age = age)
education_ct <- get_ct(educ) %>%
  rename(Education = educ)


## ---- outcome-table
# table for outcome, this table consists of 3 tables
knitr::kable(list(voted_ct, intent_ct, party_id_ct), 
             booktabs = TRUE,
             caption = "Percentage of each answer in CCES 2016. This question will be the MRP models outcome in this case study. Since the model outcome is binary, these answer will be converted to be yes/no in the context of vote for Trump/Republican.") %>%
  kable_styling() 


## ---- covariate-tables

knitr::kable(list(gender_ct, race_ct, age_ct, education_ct), 
             booktabs = TRUE,
             caption = "The response of covariates. Note that this response has been categorised into certain levels that are reflected in these tables.") %>%
  kable_styling() 

### ---- acs-response-freq

get_ct_acs <- function(x){
  acs %>% group_by({{x}}) %>%
    count() %>%
    mutate(percentage = round(n/nrow(acs)*100,2)) %>%
    select(-n)
}

age_pop <- get_ct_acs(age) %>%
  mutate(age = factor(age, levels = c("Less than 18 years", "18-24", "25-34", "35-44","45-54",
                                      "55-64", "65-74", "75-89", "90 years and over"))) %>%
  arrange(age) %>%
  rename(Age = age)
race_ethnicity_pop <- get_ct_acs(race_ethnicity) %>%
  rename(`Race and ethnicity` = race_ethnicity)
sex_pop <- get_ct_acs(sex) %>%
  rename(Sex = sex)
educ_collps_pop <- get_ct_acs(educ_collps) %>%
  mutate(educ_collps = factor(educ_collps, levels = c("No high school", 
                                               "Regular high school diploma",
                                               "Some college", 
                                               "Associate's degree", 
                                               "Bachelor's degree", 
                                               "Post-graduate"))) %>%
  arrange(educ_collps) %>%
  rename(Education = educ_collps)
  
knitr::kable(list(sex_pop, race_ethnicity_pop, age_pop, educ_collps_pop), 
             booktabs = TRUE,
             caption = "The response categories of post-stratification data.") %>%
  kable_styling() 
