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

voted_ct <- get_ct(voted_pres_16)
intent_ct <- get_ct(intent_pres_16)
party_id_ct <- get_ct(pid3_leaner)
gender_ct <- get_ct(gender)
race_ct <- get_ct(race)
age_ct <- get_ct(age)
education_ct <- get_ct(educ)


## ---- outcome-table
# table for outcome, this table consists of 3 tables

t1 <- kable(voted_ct,  col.names = c("response", "Percentage"), format = "latex", booktabs = TRUE) %>%  kable_styling(latex_options = c("striped"), font_size=5)
t2 <- kable(intent_ct,  col.names = c("response", "Percentage"), format = "latex", booktabs = TRUE) %>%  kable_styling(latex_options = c("striped"), font_size=5)
t3 <- kable(party_id_ct,  col.names = c("response", "Percentage"), format = "latex", booktabs = TRUE) %>%  kable_styling(latex_options = c("striped"), font_size=5)

## ---- sidebyside-tab

t1 <- gsub("\\begin{table}[H]", "\\begin{subtable}[b]{0.48\\linewidth}\n\\caption{\\label{tab:1a}a}\n", t1, fixed = TRUE)
t1 <- gsub("\\end{table}", "\\end{subtable}", t1, fixed = TRUE) 

t2 <- gsub("\\begin{table}[H]", "\\begin{subtable}[b]{0.48\\linewidth}\n\\caption{\\label{tab:1b}b}\n", t2, fixed = TRUE)
t2 <- gsub("\\end{table}", "\\end{subtable}", t2, fixed = TRUE)

t3 <- gsub("\\begin{table}[H]", "\\begin{subtable}[b]{0.48\\linewidth}\n\\caption{\\label{tab:1c}c}\n", t3, fixed = TRUE)
t3 <- gsub("\\end{table}", "\\end{subtable}", t3, fixed = TRUE)

## ---- tab-res
cat("",
    "\\begin{table}[!htb]",
    "\\centering",
    "\\caption{\\label{tab:tab1}a}\n",
    t1,
    t2,
    t3,
    "\\end{table}",
    "",
    sep = "\n") 

