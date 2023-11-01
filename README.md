# swasi

Welcome to the swasi, a package for working with data from the Student Wellbeing and Success Initiative (SWāSI) at the University of Oregon. This package was started by Anwesha Guha as the final capstone project for her Educational Data Science Specialization. It is a work in progress. So far, it is geared toward processing data from the baseline wave of collection. Eventually, it will cover processing end-of-year data and general visualization and analysis. Data used for this package are private.

Currently there are 3 groups of functions: cleaning.R, tables.R, and plots.R. 

The cleaning.R functions are foundational to processing SWāSI data.
* create_codebook: produces a 2-variable table with variable names and descriptions (needs work to include response options)
* clean_data: removes header rows from Qualtrics output and computes a completion_status variable (consent only, partial, complete)
* update_reference: updates the key with new arbitrary identifiers
* deidentify: deidentifies and saves original data for posterity/redundancy
* separate_consent: makes new files that identify or separate who did and did not consent to use of their data for research purposes (needs work to be more generally applicable, given variable consent procedures across the history of SWāSI)
* mini_consent: makes a file identifying who consented to contact 1) by programs and services on the basis of data provided and 2) by researchers about other research opportunities
* manipulate_text: performs some basic text manipulation that has utility across a variety of use cases

The tables.R functions produce data files that are readily usable for visualization or analysis.
* quality_control: produces data file containing variables that may be used for checking the quality of data
* intervention_essay: produces data file containing students' essays (text data) reflecting on intervention materials, intervention condition, and intervention-relevant demographics
* itemify: produces data file containing item-level data from core psychological measurements (ability uncertainty, belonging uncertainty, loneliness, social support, stereotype threat, general health, life satisfaction, stress, self-esteem, self-assurance, sadness)
* demos: produces data file containing demographics data

The plots.R functions produce rudimentary bar plots and histograms for exploratory data analysis/inspection
* bar_fun: produces bar plots of nominal variables
* hist_fun: produces histograms of oridinal or continuous variables
