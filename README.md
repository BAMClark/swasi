# swasi

Welcome to the `swasi`, a package used to tidy, analyze, and visualize data from the Student Wellbeing and Success Initiative of the Office of Assessment and Research in the Division of Student Life at the University of Oregon. 

The data files used for this package are private, though there are two simulated datasets that preserve the structure of the type of dataframes used with this package -- only with no identifiable information:

* example.csv -- this simulates the main dataframe that will be used with this package
* key_example.csv -- this simulates the key/reference file 

This package is a work in progress. It currently has six functions that collectively produce foundational files of data from the baseline wave of data collection. See documentation for more information about each function.

1. create_codebook()
2. clean_data() --> save as dataframe and feed into next two steps.
3. update_reference()
4. deidentify() --> save as dataframe and feed into next two steps.
5. separate_consent()
6. completion_status()

We are working on more functions that can be run in any order and as needed, after the first six have been run:

* quality_control()
* TBD...

## Follow along! 

Copy and paste the following the following sample code on your machine.

dat <- read.csv("example.csv")
key <- read.csv("key_example.csv")

##Step 1

create_codebook(dat, 2223) #2223 represents academic year 2022-2023

##Step 2

dat <- clean_data(dat)

##Step 3

update_reference(key, dat, 2223)

##Step 4

dat <- deidentify(key, dat, 2223)

##Step 5

separate_consent(dat, 2223)

##Step 6

completion_status(dat)

##Additional

quality_control(dat, 2223)

###If doing research, we need to make tables that include only students who have consented to participating in the research aspect of the Initiative. To do that, we join the full data set and the consent data, first reading in the "consented" file created in step 5.

consent_dat <- read.csv("consented_bl_2223.csv")
quality_control(dat, consented_dat)
