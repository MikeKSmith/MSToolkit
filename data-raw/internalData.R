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

# Create the internal dataset 'testCovariates' to test the generateData function.
#testCovariates <- read.csv(here("tests/testthat/systemTest/data/testCovariates.csv"))

# Create the internal dataset 'testParam' to test the generateData function.
#testParam <- read.csv(here("tests/testthat/systemTest/data/testParam.csv"))

# Create the testdata.datastorage folder path
testdata_datastorage_path <- here("tests/testthat/testdata.datastorage")

# Create the 'Scripts' folder path
scripts_path <- here("tests/testthat/systemTest/data/Scripts")

# Create the 'systemTest' folder path
systemTest_path <- here("tests/testthat/systemTest")

# Create the 'NONMEM' folder path
nonmem_path <- system.file(package = "MSToolkit", "tests", "testthat", "data", "NONMEM")

# Create the 'testthat' folder path
testthat_path <- system.file(package = "MSToolkit", "tests", "testthat")

usethis::use_data(microData,
                  ReplicateSample,
                  microSummary,
                  #testCovariates,
                  #testParam,
                  testdata_datastorage_path,
                  scripts_path,
                  systemTest_path,
                  nonmem_path,
                  testthat_path,
                  overwrite = TRUE, internal = TRUE)
