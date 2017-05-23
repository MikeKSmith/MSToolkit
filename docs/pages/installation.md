---
layout: page
title: Installation
description: How to install MSToolkit
---

### From GitHub
The best way to install MSToolkit currently, is to install directly from 
GitHub. You can do this very easily using the `devtools` package.
  
```
library(devtools)
install_github(repo="MikeKSmith/MSToolkit")
```
  
This will install MSToolkit into your R library.

### How to store simulation results
By default, MSToolkit writes the Replicate, Micro- and Maco-evaluation datasets
out to CSV format files, one for each simulation replicate. These are written 
to respective folders. The benefit of using CSV is the ability to easily view
the simulated data or analysis results for each replicate, as well as the ability
to share these with others. ***However***, clearly this means that there will be
three times as many CSV files as simulation replicates, which may be undesirable.

There is also the option to write the replicate data (simulated data) as 
.RData files or to keep the replicate data in memory. 
Micro- and macro-evaluation datasets are always written as CSV files.
  
```
# retrieve MSToolkit Data Method
getEctdDataMethod(method = 'CSV')  

# change the MSToolkit Data Method
setEctdDataMethod(method = 'CSV')  
# or setEctdDataMethod(method = 'RData')
# or setEctdDataMethod(method = 'Internal')  
```
  
### Parallel processing of analyzeData
The `analyzeData` function can use the `parallel` package to split analysis
across CPUs across cores on the compute resource it is running on. Specifying
`grid = TRUE` splits the job across either (number of cores - 1) or 
`getOption("max.clusters")` whichever is the minimum. It is set to `FALSE` by
default.

### SAS
Before using SAS as an analytical engine or using an external compute resource
environment, it is important to set up preferences for the location of R
and the SAS executable (if required). A file ECTD.ini is provided in the
top directory of the library which should contain the following
rows:
  
```
# Instructions to set up environmental variables

# This option allows the setting of the R execution binary on a Linux
grid
# Sys.setenv("RLSF_LINUX"=".")

# This option allows the setting of the SAS execution path on a Unix
machine
# Sys.setenv("SASPATH_UNIX"=".")

# This option allows the setting of the SAS execution path on a Windows
machine
# Sys.setenv("SASPATH_WIN"="C:\\Program Files\\SAS Institute\\SAS\\V8\\sas.exe")

```
  