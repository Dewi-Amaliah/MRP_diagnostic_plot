# All of R code used in Chapter 2 goes here

## ---- pkgs
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

## ---- read-data
# read data
papers <- read_csv(here::here("lit_rev_analysis/paper_metadata/metadata_july_22.csv"))

# number of plots 

## ---- nplots 
nplots <- nrow(papers)
p_diagplot <- round(nrow(filter(papers, Diagnostic == 1))/nplots*100,2)
p_complot <-  round(nrow(filter(papers, Diagnostic == 0))/nplots*100,2)

## ---- perform-plot
# wrangle data
paper_perf_crit <- papers %>%
  filter(performance_criteria_bias !=0 |
           performance_criteria_correlation != 0 |
           perfomance_criteria_MAE !=0 |
           performance_criteria_SE != 0 |
           `performance_criteria_MSE/RMSE` != 0) %>%
  pivot_longer(c(performance_criteria_bias,
                 performance_criteria_correlation,
                 perfomance_criteria_MAE,
                 `performance_criteria_MSE/RMSE`,
                 performance_criteria_SE), names_to = "perf_crit",
               values_to = "is_use_perf_crit") %>%
  mutate(metrics = case_when(str_detect(perf_crit, "bias") ~ "bias",
                             str_detect(perf_crit, "correlation") ~ "correlation",
                             str_detect(perf_crit, "MAE") ~ "MAE",
                             str_detect(perf_crit, "MSE") ~ "MSE/RMSE",
                             str_detect(perf_crit, "SE") ~ "SE")) %>%
  mutate(is_use_perf_crit = as.factor(is_use_perf_crit),
         metrics = factor(metrics, levels = c("MAE", "bias", "correlation", "MSE/RMSE", "SE")))

#create the plot
ggplot(paper_perf_crit, aes(fill=is_use_perf_crit, x=metrics)) + 
  geom_bar(position="stack") +
  theme() +
  xlab("performance criteria") +
  scale_fill_manual(values = c("#bababa", "#2c7bb6")) +
  theme_classic() +
  theme(legend.position = "none")

## ---- compare-plot
paper_comp <- papers %>%
  filter(comparison_MRP != 0 |
           comparison_other_method != 0 |
           comparison_other_study != 0 |
           comparison_raw != 0 |
           comparison_truth != 0 |
           comparison_weight != 0) %>%
  pivot_longer(c(comparison_MRP,
                 comparison_other_method,
                 comparison_other_study,
                 comparison_raw,
                 comparison_truth, comparison_weight),
               names_to = "comp_with",
               values_to = "is_comp_with") %>%
  mutate(comp = case_when(str_detect(comp_with, "MRP") ~ "MRP",
                          str_detect(comp_with, "method") ~ "other method",
                          str_detect(comp_with, "study") ~ "other study",
                          str_detect(comp_with, "raw") ~ "raw",
                          str_detect(comp_with, "truth") ~ "truth",
                          str_detect(comp_with, "weight") ~ "weight")) %>%
  mutate(is_comp_with = as.factor(is_comp_with),
         comp = factor(comp, levels = c("raw","truth", "MRP", "other study", "weight", "other method")))

ggplot(paper_comp, aes(fill=is_comp_with, x=comp)) + 
  geom_bar(position="stack") +
  xlab("comparison of MRP estimates with") +
  scale_fill_manual(values = c("#bababa", "#2c7bb6")) +
  theme_classic() +
  theme(legend.position = "none")

## ---- common-plots
# wrangle the data to be plotted
plot_type_sum <- papers %>%
  mutate(uncertainty = ifelse(str_detect(plot_type, "error|CI bar|CIB"), "yes", "no"),
         plot_type = as.factor(plot_type)) %>%
  mutate(plot_type_wrangled = fct_collapse(plot_type, `bar plot` = c("bar & dot plot with error bar", 
                                                                     "bar plot"),
                                           `dot plot` = c("dot plot", 
                                                          "dot plot with CI bar", 
                                                          "dot plot with CIB", 
                                                          "dot plot with error bar"),
                                           `line plot` = c("line and dot plot", 
                                                           "line and dot plot with error bar", 
                                                           "line plot"),
                                           `scatter plot` = c("scatter plot with error bar", 
                                                              "scatter plot"),
                                           `histogram & density plot` = c("density plot",
                                                                          "histogram with density plot",
                                                                          "histogram"),
                                           `other` = c("boxplot",
                                                       "bubble plot",
                                                       "heatmap",
                                                       "logit curve")
  )) %>%
  mutate(diagnostic = ifelse(Diagnostic == 0, "Communication", "Diagnostic")) %>%
  mutate(plot_type_wrangled = factor(plot_type_wrangled, levels = c("other", 
                                                                    "histogram & density plot",
                                                                    "bar plot",
                                                                    "choropleth map",
                                                                    "line plot", 
                                                                    "scatter plot",
                                                                    "dot plot")))

# create the plot
ggplot(plot_type_sum) +
  geom_bar(aes(y = plot_type_wrangled, fill = uncertainty), position="stack") +
  facet_wrap(~diagnostic) +
  theme(legend.position = "bottom") +
  ylab(" ") +
  xlab(" ") +
  scale_fill_manual(values = c("#bababa", "#2c7bb6")) +
  theme_classic()

## ---- common-axis
# filter communication plot
axis_complot <- filter(papers, Diagnostic == 0) %>% 
  group_by(x_axis, y_axis) %>% count()
# create the plot
tile_com <- ggplot(axis_complot, aes(x_axis, y_axis, fill = n)) + 
  geom_tile() +
  ylab("values in y-axis") +
  xlab("values in x-axis") +
  labs(fill = "number of plots") +
  theme_classic() +
  ggtitle("a) Communication plots") +
  theme(axis.text.x = element_text(angle = 20),
        plot.title = element_text(size = 11),
        legend.title = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))
# filter diagnostic plot
axis_diaplot <- filter(papers, Diagnostic == 1) %>% 
  group_by(x_axis, y_axis) %>% count() 
# create the plot
tile_diag <- ggplot(axis_diaplot, aes(x_axis, y_axis, fill = n)) + 
  geom_tile() +
  ylab("values in y-axis") +
  xlab("values in x-axis") +
  labs(fill = "number of plots") +
  theme_classic() +
  ggtitle("b) Diagnostic plots") +
  theme(axis.text.x = element_text(angle = 30),
        plot.title = element_text(size = 11),
        legend.title = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))
# combine the plots
tile_com + tile_diag + plot_layout(nrow = 2)



