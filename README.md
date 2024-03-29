
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MSToolkit

<!-- badges: start -->

[![R build
status](https://github.com/jluo0015/MSToolkit/workflows/R-CMD-check/badge.svg)](https://github.com/jluo0015/MSToolkit/actions)
[![Codecov test
coverage](https://codecov.io/gh/jluo0015/MSToolkit/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jluo0015/MSToolkit?branch=Dev)
[![CRAN
status](https://www.r-pkg.org/badges/version/MSToolkit)](https://CRAN.R-project.org/package=MSToolkit)
[![Codecov test
coverage](https://codecov.io/gh/MikeKSmith/MSToolkit/branch/master/graph/badge.svg)](https://app.codecov.io/gh/MikeKSmith/MSToolkit?branch=master)
<!-- badges: end -->

## Overview

The MSToolkit R package has been written to help users simulate clinical
trials, evaluate designs, analysis methodology and quantify operating
characteristics through the application of dose and study level decision
criteria.

- Can simulate parallel group, crossover and longitudinal designs.

- Flexibility in defining data generation processes using parametric
  models.

- Data generation model parameters can vary across simulation
  replicates; parameters can vary between subjects.

- The data generation function specifies the linear predictor for the
  mean response given inputs (dose, time, covariates); inverse link
  functions allow generation of continuous, binary and count data.
  User-written inverse link functions allow data generation from a
  variety of other distributions.

- Model parameters and covariates for each subject / replicate can be
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

Most users will start with the `generateData()` which generates data and
controlls dropout and missing data by specifying the design, allocation
to treatments, generation of parameters, and functional specification.
The function has a great number of arguments which control and pass
information to the low level functions.

The generateData function iteratively builds and combines the data
components for each replicate, the replicate datasets would be stored as
individual .CSV files (“replicate000x.csv”) in the “ReplicateData”
subdirectory of the working directory.

``` r
library(MSToolkit)

generateData(
  replicateN = 500, 
  subjects = 400, 
  treatDoses = c(0, 5, 25, 50, 100), 
  conCovNames = c("wt", "age"), 
  conCovMean = c(83, 55) , 
  conCovVCov = c(14,10)^2 , 
  conCovDigits = 1, 
  conCovCrit = "18 <= age <= 65", 
  genParNames = "E0,ED50,EMAX", 
  genParMean = c(2,50,10), 
  genParVCov = diag( c(.5,30,10) ), 
  genParBtwNames = "E0,ED50,EMAX", 
  genParBtwMean = c(0,0,0), 
  genParBtwVCov = diag(3), 
  respEqn = "E0 + ((DOSE * EMAX)/(DOSE + ED50))",  
  respVCov = 5, 
  interimSubj = ".3,.7")

# This example generates 500 .CSV files with 400 subjects each to store the 
# replicate datasets in the "ReplicateData" directory. 
```

Once data has been generated, the next step is to analyze the replicate
data using the `analyzeData()`. This function wraps together functions
for performing user-specified analysis on the replicate datasets and
also performing micro- and macro- level summaries of the analysis
results. The summary files are named “MicroSummary.csv” and
“MacroSummary.csv” respectively, and stored in the current R working
directory.

Besides, two directories called “MicroEvaluation” and “MacroEvaluation”
are created under the current R working directory for the output of the
analysisCode (“Micro000x.csv”) and macroCode (“Macro000x.csv”) functions
respectively.

``` r
# Standard analysis code
emaxCode <- function(data){
  library(DoseResponse)
  with( data, 
    {
    uniDoses <- sort( unique(DOSE))                                                                    
    eFit <- emaxalt( RESP, DOSE )
    outDf <- data.frame( DOSE = uniDoses, 
      MEAN = eFit$dm[as.character(uniDoses)], 
      SE = eFit$dsd[as.character(uniDoses)] )
    outDf$LOWER <- outDf$MEAN - 2 * outDf$SE
    outDf$UPPER <- outDf$MEAN + 2 * outDf$SE
    outDf$N     <- table(DOSE)[ as.character(uniDoses) ]
    outDf 
  }) 
}
             
# Macro evaluation code
macrocode <- function(data) {
  # making up a t-test
  mu0   <- data$MEAN[ data$DOSE == 0 & data$INTERIM == 0]
  mu100 <- data$MEAN[ data$DOSE == 100 & data$INTERIM == 0]
  n0    <- data$N[ data$DOSE == 0 & data$INTERIM == 0]
  n100  <- data$N[ data$DOSE == 100 & data$INTERIM == 0]
  sd0   <- data$SE[ data$DOSE == 0 & data$INTERIM == 0]
  sd100 <- data$SE[ data$DOSE == 100 & data$INTERIM == 0]
  
  sddiff <- if( n0 == n100 ){
    sqrt( (sd0^2 + sd100^2)  / (n0 + n100) )
  } else {
    sqrt( (1/n0 + 1/n100) * ( (n0-1)*sd0^2 + (n100-1)*sd100^2  ) / (n0+n100-2)  )
  }
  tstat  <- ( mu100 - mu0 ) / sddiff 
  success <- abs(tstat) > qt( .975, n0+n100-2)
  
  data.frame( SUCCESS = success, TSTAT = tstat )
}
  
# Interim analysis code
interimCode <- function( data ){
  dropdose  <- with( data, DOSE [ sign(UPPER) != sign(LOWER) & DOSE != 0] )
  outList <- list()
  if( length(dropdose) > 0 ) outList$DROP <- dropdose
  outList$STOP <- length(dropdose) == nrow(data)-1
  outList
}
   
# Run analysis
analyzeData( 1:5, analysisCode = emaxCode, macroCode = macrocode, 
  interimCode = interimCode )
```

Furthermore, The analyzeData function can use the *parallel* package to
split analysis across CPUs across cores on the compute resource it is
running on. Specifying `grid = TRUE` splits the job across either
(number of cores - 1) or `getOption("max.clusters")` whichever is the
minimum. It is set to `FALSE` by default.

## Store the simulation results

By default, MSToolkit writes the Replicate, Micro- and Maco-evaluation
datasets out to CSV format files, one for each simulation replicate.
These are written to respective folders. The benefit of using CSV is the
ability to easily view the simulated data or analysis results for each
replicate, as well as the ability to share these with others. However,
clearly this means that there will be three times as many CSV files as
simulation replicates, which may be undesirable.

There is also the option to write the replicate data (simulated data) as
.RData files or to keep the replicate data in memory. Micro- and
macro-evaluation datasets are always written as CSV files.

``` r
# retrieve MSToolkit Data Method
getEctdDataMethod(method = 'CSV')  


# change the MSToolkit Data Method
setEctdDataMethod(method = 'CSV')  
# or setEctdDataMethod(method = 'RData')
# or setEctdDataMethod(method = 'Internal')  
```

## SAS

Before using SAS as an analytical engine or using an external compute
resource environment, it is important to set up preferences for the
location of R and the SAS executable (if required). A file ECTD.ini is
provided in the top directory of the library which should contain the
following rows:

``` r
# Instructions to set up environmental variables

# This option allows the setting of the R execution binary on a Linux grid
Sys.setenv("RLSF_LINUX"=".")

# This option allows the setting of the SAS execution path on a Unix machine
Sys.setenv("SASPATH_UNIX"=".")

# This option allows the setting of the SAS execution path on a Windows machine
Sys.setenv("SASPATH_WIN"="C:\\Program Files\\SAS Institute\\SAS\\V8\\sas.exe")
```
