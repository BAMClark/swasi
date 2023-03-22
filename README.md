# swasi

Welcome to the SWASI, a package used to tidy data from the Student Wellbeing and Assessement Survey. This package is intended to serve the University of Oregon's Office of Assessment and Research in preparing SWASI files for data analysis and research projects, and the datafiles used for this package are private.
 
 This package also serves as the final capstone project for EDLD 640, so for the purposes of that class, I have also provided two simulated datasets that preserves the structure of the type of dataframes used with this package -- only with no identifiable information:

* example.csv -- this simulates the main dataframe that will be used with this package
* key_example.csv -- this simulates the key/reference file 

Note that this package is a work in progress; this package currently only works with baseline 2022-2023 data, and end-of-year data will likely come in a different structure that will compel adjusting this package. 

Also, while the entire data cleaning process could be run using one function, I break into a few functions to better diagnose if there are any errors. As a result, the functions should be run in the following order. This might be changed in future iterations to make this process more user-friendly. Though, as is, these six functions collectively produce the files currently used. I advise saving the output of each function to a dataframe, reusing that dataframe for each step. 

See documentation for more information about each function.

1. create_codebook()
2. clean_data()
3. update_reference()
4. deidentify()
5. separate_consent()
6. completion_status()

Additionally, other functions might be used to create tables for analyses and visualizations. These can be run in any order and as needed, after the previous six have been run:

* quality_control()
* TBD...
