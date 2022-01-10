
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MSToolkit

<!-- badges: start -->
<!-- badges: end -->

## Overview

The MSToolkit R package has been written to help users simulate clinical
trials, evaluate designs, analysis methodology and quantify operating
characteristics through the application of dose and study level decision
criteria.

-   Can simulate parallel group, crossover and longitudinal designs.

-   Flexibility in defining data generation processes using parametric
    models.

-   Data generation model parameters can vary across simulation
    replicates; parameters can vary between subjects.

-   The data generation function specifies the linear predictor for the
    mean response given inputs (dose, time, covariates); inverse link
    functions allow generation of continuous, binary and count data.
    User-written inverse link functions allow data generation from a
    variety of other distributions.

-   Model parameters and covariates for each subject / replicate can be
    generated from multivariate normal distributions or sampled from
    external data files e.g. existing data files.

## Installation

The best way to install MSToolkit currently, is to install the
development version directly from
[GitHub](https://github.com/MikeKSmith/MSToolkit).

``` r
#install.packages("devtools")
devtools::install_github("MikeKSmith/MSToolkit")
```

## Usage

The `generateData()` is used to specify the design, allocation to
treatments, generation of parameters, functional specification for
generating data and controlling dropout and missing data. The function
has a great number of arguments which control and pass information to
the low level functions.

The generateData function iteratively builds and combines the data
components for each replicate, the replicate datasets would be stored as
individual .CSV files in the “ReplicateData” subdirectory of the working
directory.

``` r
library(MSToolkit)
#> Loading required package: MASS
#> 
#> # MSToolkit package version 3.2.4 developed for Pfizer by Mango Solutions
#> # E-Mail: mstoolkit@googlemail.com
#> # Grid execution available: use 'grid' argument in ?analyzeData

generateData( replicateN = 500, subjects = 400, treatDoses = c(0, 5, 25, 50, 100), 
  conCovNames = c("wt", "age"), conCovMean = c(83, 55) , conCovVCov = c(14,10)^2 , 
  conCovDigits = 1, conCovCrit = "18 <= age <= 65", 
  genParNames = "E0,ED50,EMAX", genParMean = c(2,50,10), genParVCov = diag( c(.5,30,10) ), 
  genParBtwNames = "E0,ED50,EMAX", genParBtwMean = c(0,0,0), genParBtwVCov = diag(3), 
  respEqn = "E0 + ((DOSE * EMAX)/(DOSE + ED50))",  respVCov = 5, 
  interimSubj = ".3,.7")
```
