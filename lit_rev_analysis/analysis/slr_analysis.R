# All of R code used in Chapter 2 goes here

## ---- pkg-chap2
library(tidyverse)
library(kableExtra)
library(janitor)
library(patchwork)
library(ggthemes)

## ---- search-term
#table of search term used to find papers 
search <- tribble(~Database, ~`Search Terms`, ~`Search Field`, ~Inclusion, ~Exclusion, ~`Number Returned`,
                  'JSTOR', '(multilevel regression and poststratification) OR (“post-stratification”)', 'Abstract', 'Article, content I can access, English', 'anything before 1997', 44,
                  'JSTOR', '(("multilevel regression" AND ("post-stratification" OR Poststratification)) OR ("multilevel model" AND ("post-stratification" OR Poststratification)))', 'All field', 'Article, English', 'anything before 1997', 142, 
                  'EBSCO', '"multilevel regression with post-stratification" OR "multilevel regression with poststratification" OR "multilevel regression and Poststratification" OR "multilevel regression and Post-stratification"', 'All field', 'Academic (Peer-Reviewed) Journals, English', 'anything before 1997', 42,
                  'EBSCO', '(multilevel regression AND post-stratification) OR (multilevel model AND post-stratification) OR (multilevel regression AND poststratification ) OR (multilevel model AND poststratification)', 'All field', 'Academic (Peer-Reviewed) Journals, English', 'anything before 1997', 45,
                  'PubMed', '"multilevel regression with post-stratification" OR "multilevel regression with poststratification" OR "multilevel regression and Poststratification" OR "multilevel regression and Post-stratification"', 'Title/Abstract', 'Article, English', 'anything before 1997', 26,
                  'PubMed', '(multilevel regression AND post-stratification) OR (multilevel model AND post-stratification) OR (multilevel regression AND poststratification) OR (multilevel model AND poststratification)', 'All field', 'Article, English', 'anything before 1997', 28,)
# make table 
search %>% 
  kable(format = "latex", booktabs = TRUE, caption = "Detail of literature identification") %>%
  column_spec(2, width = "15em") %>% 
  kable_styling(latex_options = "scale_down") %>%
  landscape()

