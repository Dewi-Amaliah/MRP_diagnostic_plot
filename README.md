# Visualising MRP: Alternatives of The Current Practice

## :clipboard: About

This is a repository for my master degree research project about Multilevel Regression and Poststratification (MRP) visualisation. This study aims to:

1. Discuss the current practice of visualisation of MRP models.
2. Understand the implication of existing visualisation choices with real-world data.
3. Explore possible improvements of the current practice of MRP visualisation.

## :wrench: Method

According to those objectives, this study is performed through:

1. Systematic literature review on peer-reviewed articles applied MRP.
2. Case study (The application of MRP in estimating Trump's vote share in the 2016 U.S. presidential election. 

## :clipboard: Data

The data used in the case study are not uploaded in this repository but they are publicly available and can be obtained with the following steps:

### CCES data

The survey data used is the 2016 Cooperative Congressional Election Study (CCES). This data is publicly available in [Harvards dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910/DVN/GDF6Z0), but in this study it is accessed through an R-package called `ccesMRPprep`(Kuriwaki, 2021). The codes used to get the data is:

```r 
# remotes::install_github("kuriwaki/ccesMRPprep")
library(ccesMRPprep)

cces <- get_cces_dataverse("cumulative")

# filter to only have 2016 data
cces_2016 <- cces %>%
  filter(year == "2016") %>%
  ccc_std_demographics()
```

### ACS data 

This study uses 3-years of ACS data, i.e., [2015-2017 Public Use Microdata Sample (PUMS)](https://www.census.gov/programs-surveys/acs/microdata/access.2015.html), which can be obtained through:

1. [2015 PUMS data](https://www2.census.gov/programs-surveys/acs/data/pums/2015/1-Year/), download the file named: csv_pus.zip
2. [2016 PUMS data](https://www2.census.gov/programs-surveys/acs/data/pums/2016/1-Year/), download the file named: csv_pus.zip
3. [2017 PUMS data](https://www2.census.gov/programs-surveys/acs/data/pums/2017/1-Year/), download the file named: csv_pus.zip

The complete codes to do data preparation is available in [this file](https://github.com/Dewi-Amaliah/MRP_diagnostic_plot/tree/main/case_study/analysis/analysis_code). 

## :file_folder: File Structure

The file stucture of this repository is:

#### :card_index_dividers: case_study

This folder includes all of the files needed to reproduce the analysis in the case study (assume that the downloaded ACS data is saved in folder data/data_raw). 

  - analysis
    - analysis_code (folder contains codes used in the case study)
      - cces_acs_wrangling.R (data wrangling of CCES and ACS data)
      - mrp_build.Rmd (code used when prepare and fitting MRP models)
      - mrp_fitting.R (code used to prepare and specifiy MRP models, used in the report for Chapter 3)
      - mrp_vis.R (code used to visualise MRP estimatesm used in report for Chapter 3)
    - hpc_code (folder contains R codes and slurm submission for High Performance Cluster)
      - The results of model fitting in the HPC are saved in a folder called results.
        
#### :card_index_dividers: presentation
  This folder contains slides for presenting the thesis (preliminary presentation and final presentation).
  Slides are made using Xaringan in `rmarkdown`.

#### :card_index_dividers: report
  This folder contains the thesis/report. The full report (pdf version) is located in folder: "_book"
  
#### :card_index_dividers: sys_literature_review

This folder includes all of the files needed to reproduce the systematic literature review in this study.
  
  - analysis (folder contains the analysis of extracted data from systematic literature review).
    - preliminary_result.Rmd (rough result of systematic literature review)
    - slr_analysis.R (code used in Chapter 2)
  - endnote_lib
    - This folder contains endnote files to manage reviewed literatures. 
  - lit_review_documentation
    - Database_search.docx (documentation of paper identification in each research database)
    - inclusion_mismatch_notes.docx (documentation of screening mismatch by DA and LK)
  - paper_metadata
    - Folder contains csv files of papers' metadata extracted from slr. 

## :wrench: Packages used 

If you want to reproduce this analysis, make sure you have these packages installed:
`mrpkit`; `ccesMRPprep` ; `brms`; `cmdstanr`; `survey`; `ddi`; `tidyverse`; `forcats`; `Metrics`; `data.table`; `kableExtra`; `janitor`; `scales`; `ggplot2`; `patchwork`; `flipPlots`; `igraph`; `urbnmapr`; `ggstance`; `ggpmisc`; `wacolors`; `rmarkdown`; `knitr`; `MonashEBSTemplates`.


