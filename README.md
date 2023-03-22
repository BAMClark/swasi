# swasi

Welcome to the SWASI, a package used to tidy data from the Student Wellbeing and Assessement Survey. This package is intended to serve the University of Oregon's Office of Assessment and Research in preparing SWASI files for data analysis and research projects, and the datafiles used for this package are private.
 
 This package also serves as the final capstone project for EDLD 640, so for the purposes of that class, I have also provided two simulated datasets that preserves the structure of the type of dataframes used with this package -- only with no identifiable information:

* example.csv -- this simulates the main dataframe that will be used with this package
* key_example.csv -- this simulates the key/reference file 

Note that this package is a work in progress; this package currently only works with baseline 2022-2023 data, and end-of-year data will likely come in a different structure that will compel adjusting this package. 

Also, while the entire data cleaning process could be run using one function, I break into a few functions to better diagnose if there are any errors. As a result, the functions should be run in the following order. This might be changed in future iterations to make this process more user-friendly. Though, as is, these six functions collectively produce the files currently used.

See documentation for more information about each function.

1. create_codebook()
2. clean_data() --> save as dataframe and feed into next two steps.
3. update_reference()
4. deidentify() --> save as dataframe and feed into next two steps.
5. separate_consent()
6. completion_status()

Additionally, other functions might be used to create tables for analyses and visualizations. These can be run in any order and as needed, after the previous six have been run:

* quality_control()
* TBD...

Copy and paste the following the following sample code on your machine.

dat <- read.csv("example.csv")
key <- read.csv("key_example.csv")

# Step 1
create_codebook(dat, 2223) #2223 represents academic year 2022-2023

# Step 2
dat <- clean_data(dat)

# Step 3
update_reference(key, dat, 2223)

# Step 4
dat <- deidentify(key, dat, 2223)

# Step 5
separate_consent(dat, 2223)

# Step 6
completion_status(dat)

# Additional
quality_control(dat, 2223)

## if interested in creating quality control tables only on consented inviduals, we can left join on that dataset using the following code (you first need to read in the dataset you created in step 5

consent_dat <- read.csv("consented_bl_2223.csv")
quality_control(dat, consented_dat)
