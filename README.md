# Visualising MRP: Alternative of The Current Practice

## :clipboard: About

This is a repository for my master degree thesis about Multilevel Regression and Poststratification (MRP) visualisation. This study aims to:

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



The complete code to do data preparation is available in [this file](https://github.com/Dewi-Amaliah/MRP_diagnostic_plot/blob/main/case_study/analysis/cces_acs_wrangling.Rmd). 

## :file_folder: File Structure

The file stucture of this repository is:

#### :card_index_dividers: case_study

This folder includes all of the files needed to do case study.
  - analysis
    - cces_acs_wrangling.R (data wrangling of CCES and ACS data)
    - mrp_build.R (code to build MRP model)
    - mrp_visualisation.R (code to visualise MRP estimates)
    - hpc_code (folder contains R codes and slurm submission for High Performance Cluster)
    
  Note: 
  - Wrangled data and raw data are not published in this repository with respect to data publication license. However steps to access the data would be discussed in the report (chapter 3).
  - results folder is not included in this repository due to size limit. Code to produce the file in results is accessable and reproducible in case_study/mrp_build.R
  
#### :card_index_dividers: endnote_lib

This folder contains endnote files to manage reviewed literatures. 
  
#### :card_index_dividers: lit_review_analysis
  This folder contains the analysis of extracted data from systematic literature review. 
  - analysis
    - preliminary_result.Rmd (rough result of systematic literature review)
    - preliminary_result.html (html version)
    - slr_analysis.R (code used in Chapter 2)
  - paper_metadata
    Folder contains csv files of papers' metadata extracted from slr. 

#### :card_index_dividers: lit_review_documentation
  - Database_search.docx (documentation of paper identification in each research database)
  - inclusion_mismatch_notes.docx (documentation of screening mismatch by DA and LK)

#### :card_index_dividers: presentation
  This folder contains slides for presenting the thesis (preliminary presentation and final presentation).
  Slides are made using Xaringan. 

#### :card_index_dividers: report
  This folder contains the thesis/report. The full report (pdf version) is located in _book folder.
