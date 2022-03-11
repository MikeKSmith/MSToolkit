## code to prepare `internalData` dataset goes here

# These codes are used to create the internal data for the function samples or testing

library(tidyverse)
library(here)

# create a R script to access the data for the example of macroEvaluation function
microData <- read.csv(here("tests/testthat/data/macroEvaluation/micro0001.csv"), header = TRUE)

# Create this dataset to provide the data sample for testing the writeData/readData function.
ReplicateSample <- read.csv(here("tests/testthat/testdata.datastorage/ReplicateSample.csv"))

# Create this dataset to provide test data for testing readData function (test.readData.CSV)
microSummary <- read.csv(here("tests/testthat/testdata.datastorage/microSummary.csv"))

# Create the testdata.datastorage folder path
testdata_datastorage_path <- here("tests/testthat/testdata.datastorage")

# Create the 'Scripts' folder path
scripts_path <- here("tests/testthat/systemTest/data/Scripts")

# Create the 'systemTest' folder path
systemTest_path <- here("tests/testthat/systemTest")

usethis::use_data(microData,
                  ReplicateSample,
                  microSummary,
                  testdata_datastorage_path,
                  scripts_path,
                  systemTest_path,
                  overwrite = TRUE, internal = TRUE)
