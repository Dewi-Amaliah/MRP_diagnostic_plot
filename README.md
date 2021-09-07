# MRP Diagnostic Visualisation

## :clipboard: About

This is a repository for my master degree thesis about Multilevel Regression with Poststratification (MRP) diagnostic visualisation. This study aims to:

1. Understand the current practice of MRP model visualisation. 
2. Understand the implication of existing viusalisation choices with real-world data.
3. Improve the common practices of MRP visualisations.

## :wrench: Method
According to those objectives, this study is performed through:

1. Systematic literature review
2. Case study

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
