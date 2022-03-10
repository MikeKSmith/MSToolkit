## code to prepare `microData` dataset goes here

# These codes are used to create the internal data for the function samples or testing

library(tidyverse)
library(here)

# create a R script to access the data for the example of macroEvaluation function
microData <- read.csv(here("tests/testthat/data/macroEvaluation/micro0001.csv"))
# Create this dataset to provide the data sample for testing the writeData function.
ReplicateSample <- read.csv(here("tests/testthat/testdata.datastorage/ReplicateSample.csv"))

usethis::use_data(microData, ReplicateSample, overwrite = TRUE, internal = TRUE)
