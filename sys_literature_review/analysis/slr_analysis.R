# All of R code used in Chapter 2 goes here

## ---- pkgs
library(tidyverse)
library(kableExtra)
library(janitor)
library(patchwork)
library(flipPlots)
library(igraph)

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
papers <- read_csv(here::here("sys_literature_review/paper_metadata/metadata_july_22.csv"))

# number of plots 

## ---- nplots 
nplots <- nrow(papers)
p_diagplot <- round(nrow(filter(papers, Diagnostic == 1))/nplots*100,2)
p_complot <-  round(nrow(filter(papers, Diagnostic == 0))/nplots*100,2)
n_diagplot <- nrow(filter(papers, Diagnostic == 1))

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

n_perf <- papers %>%
  filter(performance_criteria_bias !=0 |
           performance_criteria_correlation != 0 |
           perfomance_criteria_MAE !=0 |
           performance_criteria_SE != 0 |
           `performance_criteria_MSE/RMSE` != 0) %>%
  nrow()
  
#create the plot
ggplot(paper_perf_crit, aes(fill=is_use_perf_crit, x=metrics)) + 
  geom_bar(position="stack") +
  theme() +
  xlab("Performance criteria") +
  ylab("Count") +
  scale_fill_manual(values = c("#bababa", "#2c7bb6")) +
  theme_bw() +
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

# number of plots display comparison
n_comp <- papers %>%
  filter(comparison_MRP != 0 |
           comparison_other_method != 0 |
           comparison_other_study != 0 |
           comparison_raw != 0 |
           comparison_truth != 0 |
           comparison_weight != 0) %>%
  nrow()


ggplot(paper_comp, aes(fill=is_comp_with, x=comp)) + 
  geom_bar(position="stack") +
  xlab("Comparison of MRP estimates with") +
  ylab("Count") +
  scale_fill_manual(values = c("#bababa", "#2c7bb6")) +
  theme_bw() +
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
  theme_bw()

## ---- common-axis
# filter communication plot
papers$y_axis[is.na(papers$y_axis)] <- "not used"

axis_complot_orig <- filter(papers, Diagnostic == 0) %>% 
  group_by(x_axis, y_axis) %>% 
  count() %>%
  ungroup()

axis_complot <- filter(papers, Diagnostic == 0) %>% 
  group_by(x_axis, y_axis) %>% 
  count() %>%
  ungroup() %>%
  graph.data.frame(directed=FALSE) %>%
  get.adjacency(attr="n", sparse=FALSE) %>%
  as.data.frame() %>%
  rownames_to_column("x_axis") %>%
  pivot_longer(c(-1), names_to = "y_axis", values_to = "n") %>%
  select(x_axis, y_axis)

axis_commplot <- left_join(axis_complot, axis_complot_orig, by = c("x_axis", "y_axis"))


# create the plot
tile_com <- ggplot(axis_commplot, aes(x_axis, y_axis, fill = n)) + 
  geom_tile() +
  geom_text(aes(label = n),
            color = "white") +
  ylab("Values in y-axis") +
  xlab("Values in x-axis") +
  labs(fill = "number of plots") +
  theme_bw() +
  ggtitle("a) Communication plots") +
  theme(axis.text.x = element_text(angle = 20, size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 11),
        legend.title = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.position = "none") +
  scale_fill_continuous(na.value = "white")

# filter diagnostic plot
axis_diaplot_orig <- filter(papers, Diagnostic == 1) %>% 
  group_by(x_axis, y_axis) %>% 
  count() %>%
  ungroup() 


axis_diaplot <- filter(papers, Diagnostic == 1) %>% 
  group_by(x_axis, y_axis) %>% 
  count() %>%
  ungroup() %>%
  graph.data.frame(directed=FALSE) %>%
  get.adjacency(attr="n", sparse=FALSE) %>%
  as.data.frame() %>%
  rownames_to_column("x_axis") %>%
  pivot_longer(c(-1), names_to = "y_axis", values_to = "n") %>%
  select(x_axis, y_axis)

axis_diagplot <- left_join(axis_diaplot, axis_diaplot_orig, by = c("x_axis", "y_axis"))

# create the plot
tile_diag <- ggplot(axis_diagplot, aes(x_axis, y_axis, fill = n)) + 
  geom_tile() +
  geom_text(aes(label = n),
            color = "white") +
  ylab("Values in y-axis") +
  xlab("Values in x-axis") +
  labs(fill = "number of plots") +
  theme_bw() +
  ggtitle("b) Diagnostic plots") +
  theme(axis.text.x = element_text(angle = 15, size = 6),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 11),
        legend.title = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.position = "none") +
  scale_fill_continuous(na.value = "white")

# combine the plots
tile_com + tile_diag + plot_layout(nrow = 2)


## ---- facet-plots
filter_facet_all <- papers %>%
  filter(Facets > 1) %>%
  pivot_longer(c(Facet_x, Facet_y), names_to = "facet_axis", values_to = "facet_type") %>%
  group_by(facet_type) %>%
  count()

ggplot(filter(filter_facet_all, !is.na(facet_type))) +
  geom_bar(aes(x = reorder(facet_type, -n), y = n), stat = "identity", fill = "#2c7bb6") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 10, size = 7)) +
  xlab("What is in the facet") +
  ylab("Count") 

## ---- sankey-feature

papers$color[is.na(papers$color)] <- "Not used"
papers$shape[is.na(papers$shape)] <- "Not used"

papers_sum <- papers %>%
  rename(aim = Diagnostic) %>%
  mutate(aim = case_when(aim == 1 ~ "diagnostic",
                         aim == 0 ~ "communication")) %>%
  group_by(aim, color, shape) %>%
  count()

SankeyDiagram(papers_sum[, -4],
              link.color = "Source", 
              weights = papers_sum$n,
              font.size = 8,
              font.family = "Arial",) 



