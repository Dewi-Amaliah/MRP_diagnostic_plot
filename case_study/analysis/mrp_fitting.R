# All of R code used in Chapter 3 until model fitting goes here

## ---- pkgs-chap3
library(mrpkit)
library(brms)
library(tidyverse)
library(ggplot2)
library(urbnmapr)
library(kableExtra)
library(patchwork)
library(Metrics)
library(survey)
library(ggstance)
library(data.table)
library(ggpmisc)
library(forcats)

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
state_ct <- get_ct(state) %>%
  rename(State = state)


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

## ---- state-cces

state_ct %>%
  ungroup() %>%
  mutate(State = fct_reorder(State, percentage)) %>%
  ggplot(aes(x=State, y=percentage)) +
  geom_bar(stat="identity", fill="grey50", alpha=.6, width=.4) +
  coord_flip() +
  xlab("State") +
  ylab("Percentage") +
  theme_bw()

## ---- acs-response-freq

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


## ---- state-acs

get_ct_acs(state) %>%
  rename(State = state) %>%
  ungroup() %>%
  mutate(State = fct_reorder(State, percentage)) %>%
  ggplot(aes(x=State, y=percentage)) +
  geom_bar(stat="identity", fill="grey50", alpha=.6, width=.4) +
  coord_flip() +
  xlab("State") +
  ylab("Percentage") +
  theme_bw()

## ---- survey-pop-data

# pre-process the cces data for base model
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

#### CREATE SURVEYDATA OBJECT
# create survey data object for the survey data
cces_data_obj <- SurveyData$new(
  data = cces_mrp,
  questions = list(age = "Respondent's age group",
                   state = "State",
                   race_ethnicity_collp = "Respondent's race/ethnicity collapsed Native American, Middle Eastern, and Other to Other",
                   race = "Respondent's race/ethnicity original level",
                   gender = "Respondent's gender",
                   educ = "Respondent's education",
                   vote = "Whether the respondent vote for Donald Trump in 2016 Presidential vote (Post)",
                   intent = "Whether the respondent intent to vote for Donald Trump in 2016 Presidential vote",
                   party = "Whether the respondent identify him/herself as Republican or not"),
  responses = list(age = levels(cces_mrp$age),
                   state = levels(cces_mrp$state),
                   race_ethnicity_collp = levels(cces_mrp$race_ethnicity_collp),
                   race = levels(cces_mrp$race),
                   gender = levels(cces_mrp$gender),
                   educ = levels(cces_mrp$educ),
                   vote = c("no", "yes"),
                   intent = c("no", "yes"),
                   party = c("Republican", "not Republican")),
  weights = "wt")
# this step: 0.039 sec elapsed


# pre-process the acs data for base model
acs_mrp <- acs %>%
  # filter out the respondents whose age < 18 years because they are not in the scope of CCES survey?
  filter(age != "Less than 18 years") %>%
  select(state, wt, age, sex, race_ethnicity, race_ethnicity_collps, educ_collps)

# drop unused level in age variable
acs_mrp$age <- droplevels(acs_mrp$age)

# create survey data object for the post-stratification data 
acs_data <- SurveyData$new(
  data = acs_mrp,
  questions = list(age = "Respondent's age group",
                   state = "State",
                   sex = "Respondent's gender",
                   race_ethnicity_collps = "Respondent's race/ethnicity recoded",
                   race_ethnicity = "Respondent's race/ethnicity",
                   educ_collps = "Respondent's highest educational attainment"),
  responses = list(age = levels(acs_mrp$age),
                   state = levels(acs_mrp$state),
                   sex = levels(acs_mrp$sex),
                   race_ethnicity_collps = levels(acs_mrp$race_ethnicity_collps),
                   race_ethnicity = levels(acs_mrp$race_ethnicity),
                   educ_collps = levels(acs_mrp$educ_collps)),
  weights = "wt"
)

## ---- questionmap

q_age <- QuestionMap$new(
  name = "age",
  col_names = c("age", "age"),
  values_map = list(
    "18 to 24 years" = "18-24",
    "25 to 34 years" = "25-34",
    "35 to 44 years" = "35-44",
    "45 to 64 years" = "45-54",
    "45 to 64 years" = "55-64",
    "65 years and over" = "65-74",
    "65 years and over" = "75-89",
    "65 years and over" = "90 years and over"))

q_state <- QuestionMap$new(
  name = "state",
  col_names = c("state", "state"),
  values_map = list(
    "Alabama" = "Alabama",
    "Alaska" = "Alaska",
    "Arizona" = "Arizona",
    "Arkansas" = "Arkansas",
    "California" = "California",
    "Colorado" = "Colorado",
    "Connecticut" = "Connecticut",
    "Delaware" = "Delaware",
    "District of Columbia" = "District of Columbia",
    "Florida" = "Florida",
    "Georgia" = "Georgia",
    "Hawaii" = "Hawaii",
    "Idaho" = "Idaho",
    "Illinois" = "Illinois",
    "Indiana" = "Indiana",
    "Iowa" = "Iowa",
    "Kansas" = "Kansas",
    "Kentucky" = "Kentucky",
    "Louisiana" = "Louisiana",
    "Maine" = "Maine",
    "Maryland" = "Maryland",
    "Massachusetts" = "Massachusetts",
    "Michigan" = "Michigan",
    "Minnesota" = "Minnesota",
    "Mississippi" = "Mississippi",
    "Missouri" = "Missouri",
    "Montana" = "Montana",
    "Nebraska" = "Nebraska",
    "Nevada" = "Nevada",
    "New Hampshire" = "New Hampshire",
    "New Jersey" = "New Jersey",
    "New Mexico" = "New Mexico",
    "New York" = "New York",
    "North Carolina" = "North Carolina",
    "North Dakota" = "North Dakota",
    "Ohio" = "Ohio",
    "Oklahoma" = "Oklahoma",
    "Oregon" = "Oregon",
    "Pennsylvania" = "Pennsylvania",
    "Rhode Island" = "Rhode Island",
    "South Carolina" = "South Carolina",
    "South Dakota" = "South Dakota",
    "Tennessee" = "Tennessee",
    "Texas" = "Texas",
    "Utah" = "Utah",
    "Vermont" = "Vermont",
    "Virginia" = "Virginia",
    "Washington" = "Washington",
    "West Virginia" = "West Virginia",
    "Wisconsin" = "Wisconsin",
    "Wyoming" = "Wyoming"))

q_gender <- QuestionMap$new(
  name = "gender",
  col_names = c("gender", "sex"),
  values_map = list(
    "Male" = "Male",
    "Female" = "Female"))

q_recollapsed <- QuestionMap$new(
  name = "collapsed_re",
  col_names = c("race_ethnicity_collp", "race_ethnicity_collps"),
  values_map = list(
    "Asian" = "Asian alone",
    "Black" = "Black or African American alone",
    "Hispanic" = "Hispanic",
    "Other" = "Other",
    "Other" = "Two or More Races",
    "White" = "White alone"))

q_re <- QuestionMap$new(
  name = "original_re",
  col_names = c("race", "race_ethnicity"),
  values_map = list(
    "Black" = "Black or African American alone",
    "White" = "White alone",
    "Hispanic" = "Hispanic",
    "All Other" = "Some Other Race alone",
    "Native American" = "American Indian and Alaska Native tribes",
    "All Other" = "Two or More Races",
    "Asian" = "Asian alone",
    "Native American" = "American Indian alone",
    "All Other" = "Native Hawaiian and Other Pacific Islander alone",
    "Native American" = "Alaska Native alone"))


q_educ <- QuestionMap$new(
  name = "education",
  col_names = c("educ", "educ_collps"),
  values_map = list(
    "HS or Less" = "No high school",
    "HS or Less" = "Regular high school diploma",
    "Some College" = "Some college",
    "Some College" = "Associate's degree",
    "4-Year" = "Bachelor's degree",
    "Post-Grad" = "Post-graduate"))

## ---- tabulation
map_model <- SurveyMap$new(sample = cces_data_obj,
                           population = acs_data,
                           q_age,
                           q_state,
                           q_gender,
                           q_recollapsed, 
                           q_re,
                           q_educ)

map_model$mapping()
map_model$tabulate()

#saveRDS(map_model, here::here("case_study/results/map_model.rds"))

## ---- vote-dist
get_ct_bin <- function(x){
  cces_mrp %>% group_by({{x}}) %>%
    count() %>%
    mutate(percentage = round(n/nrow(cces)*100,2)) %>%
    select(-n)
}

get_ct_bin(vote) %>%
  rename(`Candidate voted` = vote) %>%
  kable(caption = "The distribution of answer in the outcome (vote). It will be the outcome in three models, i.e., baseline model, model with education as additional covariate, and model with more categories in race. We observe a reasonably large percentage of NA.",
        booktabs = TRUE) %>%
  kable_styling()
  
## ---- intent-dist

get_ct_bin(intent) %>%
  rename(`Candidate will be voted` = intent) %>%
  kable(caption = "The distribution of answer in the outcome (intent).",
        booktabs = TRUE) %>%
  kable_styling()

## ---- party-dist

get_ct_bin(party) %>%
  rename(`Party identity` = party) %>%
  kable(caption = "The distribution of answer in the outcome (party).",
        booktabs = TRUE) %>%
  kable_styling()


## ---- post-strat-table
map_model<- readRDS(here::here("case_study/results/map_model.rds"))

map_model$poststrat_data() %>%
  head(n = 5) %>%
  kable(caption = "First five rows of the post-stratification table", booktabs = TRUE) %>% 
  kable_styling()



