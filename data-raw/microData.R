## code to prepare `microData` dataset goes here

library(tidyverse)
# create a R script to access the data for the example of macroEvaluation function
microData <- read.csv(here::here("tests/testthat/data/macroEvaluation/micro0001.csv"))


usethis::use_data(microData, overwrite = TRUE, internal = TRUE)
