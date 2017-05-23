---
layout: page
title: Simple example - Emax dose-response 
description: Illustrating MSToolkit use for evaluating operating characteristics
of a dose-response design fitting a "step-down" Emax -> Log-linear -> Linear
approach to analysis, and looking for model-based estimates of effect at each
dose.
---

## Data generation

### Design
The design for this study is as follows:
  - 100 subjects
  - Parallel group
  - 5 doses: Placebo, 5, 10, 50, 100mg (units are arbitrary in this example)
  - Patients are allocated randomly to each dose i.e. we do not guarantee equal
  allocation to each treatment arm.
  - Interim analyses after 30% and 70% of subjects have provided response

### Response generation function
  - Emax model: RESP = E0 + (EMAX * DOSE) / (ED50 + DOSE)
    * E0 ~ N(2,0.3)
    * ED50 ~ N(50,30)
    * EMAX ~ N(10,10)
  - Residual error ~ N(0,2)
    * Additive i.e. Y = RESP + residual error
  
### Simulation specification
  - 5 trial replicates
  - Simulate with model parameter uncertainty i.e. each replicate has unique
  values for model parameters, drawn from an underlying multivariate distribution.
  - In this case, model parameters are independent i.e. off-diagonal elements of
  the multivariate variance-covariance matrix are zero.

For more explanation of why you might vary model parameter values
between replicates please read the 
[simulation overview](pages/simulation_overview.html).

### MSToolkit generateData code
  
```
generateData(
  replicateN = 5,
  subjects = 100,
  treatDoses = c(0, 5, 10, 50, 100),
  genParNames = "E0,ED50,EMAX",
  genParMean = c(2, 50, 10),
  genParVCov = c(.5, 30, 10),
  respEqn = "E0 + ((DOSE * EMAX)/(DOSE + ED50))",
  respVCov = 2,
  interimSubj = ".3,.7"
)
```
  
## Analysis

Full code for data generation and analysis of this example can be viewed
on the [Emax R code](pages/emax_r_code.html) page.
  
```
emaxCode <- function(data) {
  uniDoses <- sort(unique(data$DOSE))
  obsMean <- tapply(data$RESP, list(data$DOSE), mean)
  obsSD <-
    tapply(data$RESP, list(data$DOSE), sd)
  eFit <- emax.fit(data$RESP, data$DOSE)
  outDf <- data.frame(
    DOSE = uniDoses,
    MEAN = eFit$fitpred,
    SE = eFit$sdpred,
    SDDIF = eFit$sddif
  )
  outDf$LOWER <- outDf$MEAN - 1.96 * outDf$SE
  outDf$UPPER <- outDf$MEAN + 1.96 * outDf$SE
  outDf$N     <- table(data$DOSE)
  outDf$OBSMEAN <- obsMean
  outDf$OBSSD <- obsSD
  outDf
}
```
  

The example code above is using the function `emax.fit` to
perform the basic analysis. This function allows access to many
summaries of the model that is fit to the data. 
The replicate datasets are passed to the function through the argument `data`.

Analysis output (`micro000x.csv` micro-evaluation datasets) are stored
in the **MicroEvaluation** sub-directory within the working directory.
One dataset is stored for each replicate. 
The [`analyzeData`](pages/analyzedata.html) function
collates the individual micro000x.csv files into a single **MicroSummary**.csv 
data file after the last replicate is analyzed. This **MicroSummary**.csv file 
is stored at the top level of the working directory. 

### Interim analysis
**NOTE** that you need to specify interim analysis proportions in the
generated data BEFORE you analyze the replicate dataset. If subjects are
not assigned to interim cuts in the generated data, the interim analysis
steps will not be carried out (the `analyzeData` function assumes that all
subjects are in the FULL dataset).
  
```
interimCode <- function(data) {
  # DROP any doses where the lower bound of the difference from placebo is negative
  dropdose  <- with(data , DOSE [LOWER < 0 & DOSE != 0])
  outList <- list()
  if (length(dropdose) > 0)
    outList$DROP <- dropdose
  outList$STOP <- length(dropdose) == nrow(data) - 1
  outList
}
```
  
The `interimcode` function above will be run **after** the
`analysisCode` at each specified interim analysis and the function
specifies rules for dropping doses or terminating the study.
**REQUIRED** outputs are **DROP** and **STOP**.  
The code above checks whether the lower confidence limit for the difference 
from placebo for each dose is below zero i.e. no difference from placebo. 
The DROP variable is a vector of those doses which are to be dropped from the
study. STOP is a flag which indicates whether the study should stop. In
this case, if all doses are included in DROP (not including placebo)
then the study is stopped.

The micro-evaluation dataset **micro000x**.csv found in the
**MicroEvaluation** sub-directory (see below) compiles together results from the
analysis of each interim. It contains a column INTERIM which indicates
which interim analysis the results pertain to. INTERIM==0 denotes the
analysis of the FULL dataset i.e. if no doses are dropped and assuming
the study goes on to completion. This allows us to compare an adaptive
design against a non-adaptive design in one step. If, after interim
evaluation we drop a dose, then DROPPED=1 for this dose and subjects on
this dose are not included in evaluation at the subsequent interims.
Information on that dose already gained will be carried forward into
subsequent interim analysis and updated with the new model evaluation,
but the dose will remain closed for allocation. The **micro000x**.csv
dataset also includes the STOPPED variable which indicates that at a
specific interim analysis the study was stopped. Note that we perform
the analysis of the FULL (100%) dataset BEFORE conducting the interim
analyses so that if the decision is made to stop the study we can
compare results against the comparable data without interim analysis.
  
## Evaluation of operating characteristics
Operating characteristics here refer to the performance of the given design +
analysis + decision criteria. We are usually interested in the probability of 
making correct decisions (correct GO and correct NO GO) and the probabilities of
making false positive or false negative decisions separately. Many factors can
influence these probabilities:
  - dose-selection and numbers of subjects influence the precision of our 
  estimated parameters
  - the choice of model and model selection process
  - how we handle estimation when the observed data do not match our *a priori*
  model selection
  - our decision criteria and whether this is based purely on the estimated mean
  or whether it involves interval estimates.

### Micro-evaluation of *a priori* effect vs simulated / "observed"
With MSToolkit we introduce the idea of micro-evaluation (a treatment specific
comparison of *a priori* known effects or "truth" vs simulated trial, "observed"
or estimated effects). Micro-evaluation will help characterise bias and 
precision of estimated parameters or treatment effects. We use this to help 
refine the analytical methods - we should aim to reduce bias and increase 
precision of our estimates for a given data generation + analysis method. Note 
that it is good practice to investigate micro-evaluation properties for "null"
data generation cases i.e. where there is *a prior* no effect. We usually also
look at sensitivity to *a priori* assumptions. We can do this by varying the 
data generation model compared to the assumptions made by the analysis method.
This can include generating data from non-monotonic increasing functions when
analysis assumes monotonically increasing dose-response functions.

### Macro-evaluation to assess decision criteria
MSToolkit also uses what we call "macro-evaluation" to assess the operating 
characteristics of our decision criteria. Macro-evaluation aims to make an 
overall decision about the "success" or "failure" of a trial. Usually the 
macro-evaluation decision criteria compares the decision made for the simulated 
trial data with the decision that would have been made given perfect information
or the *a prior* known "truth".
  
```
macroCode <- function(data) {
  # Is effect at highest dose significant?
  success <-
    data$LOWER[data$INTERIM == max(data$INTERIM) &
                 data$DOSE == max(data$DOSE)] > 7
  data.frame(SUCCESS = success)
}
```
  
The above macro-evaluation code looks at the lower confidence limits of the
difference over placebo and if all of these values are less than zero
declares the trial a failure. If at least ONE of these confidence limits
are above zero then the trial is deemed a success. The
**MacroEvaluation** directory contains the results of macro-evaluation
(`macroCode` output) for each replicate in the **macro000x**.csv
dataset. The [`analyzeData`](pages/analyzedata.html) function
compiles these into a **MacroSummary**.csv file which is stored at the
top level of the working directory.  

The `macroCode` function can return any macro-evaluation summary that is useful 
for assessing the trial-level operating characteristics e.g. Success/Failure, 
bias and precision of parameter estimates, etc.

## Running analysis using [`analyzeData`](pages/analyzedata.html)
  
```
analyzeData(analysisCode = emaxCode,
            macroCode = macroCode,
            interimCode = interimCode)
```
  
`analysisCode` and `macroCode` are **REQUIRED** inputs.

[`analyzeData`](pages/analyzedata.html) wraps the
analysis & micro-evaluation, macro-evaluation and interim analysis
functions together and controls the input / output of replicate data
(**replicate000x**.csv from the **ReplicateData** subdirectory), passing
this to the analysis function (as argument **data**) and returning the
micro-evaluation output **micro000x**.csv for each replicate to the
**MicroEvaluation** subdirectory. It also applies the macro-evaluation
function to the micro000x dataset, generating a **macro000x**.csv
dataset which is stored in the **MacroEvaluation** subdirectory. Should
interim analysis be requested, the **analyzeData** function will apply
the **analysisCode** function first to the full dataset, then to each
interim cut of the data. For each interim cut (not including the FULL
dataset analysis) the **interimCode** function will be applied to decide
whether to DROP doses or STOP the study.

If using SAS code to perform analysis, the code **MUST** be placed at
the **TOP** level of the working directory. The code should be written
to accept a working input dataset called `INFILE` and should return a
final dataset of results called `OUTFILE`. Both of these datasets
should be contained in the `WORK` directory (i.e. **NOT** permanent
SAS datasets). Code should be written to be robust to errors. It is the
user's responsibility to track errors within the SAS code. 

To call external SAS code for analysis the following syntax should be
used:
  
```
analyzeData(analysisCode = "emax.sas", 
            software = "SAS", 
            macroCode = macrocode)
```
  
`macroCode` functions can be written in R. The SAS output
will be passed back into R and macro-evaluation carried out on each
replicate as though the analysis had been carried out in R.

For the above example we ran the following SAS analysis code. This code
analyses the generated data using a basic `PROC NLIN` call. 
In this example we are only interested in assessing the model fit parameters 
against the "true" values used in data generation. We use the SAS ODS system to 
create a dataset of the model parameters.
  
```
ods output parameterestimates=parms corrb=corr;
proc nlin;
 model resp=e0 + (emax * dose) / (ed50 + dose);  ** Specify the form of the Emax equation;
 parameters e0 = 0 ed50 = 25 emax = 120;  ** Specify starting values for the parameters;
 bounds ed50 > 0;   ** Set up boundary conditions.  Here ED50 must be positive;
run;
ods output close;
proc transpose data=parms out=tparms prefix=mean;
 var estimate;
 id parameter; 
run;
proc transpose data=parms out=stderr prefix=se;
 var estimate;
 id parameter; 
run;
proc sql noprint;
 create table doses as
   select unique(dose) as dose
   from infile;
 create table parms2 as
   select * from tparms t, stderr s;
 create table doseparms as
   select * from doses d, tparms t, stderr s;
 create table obsvars as
   select mean(resp) as dsm, var(resp) as dsv,count(resp) as n
   from infile
   group by dose;
quit;
data outfile (drop=_name_);
 retain mean se lower upper 0;
 set doseparms;
 mean=0;
 se=0;
 lower=0;
 upper=0;
 n=0;
 run;
```
  

## Troubleshooting the analysis code

To troubleshoot analysis / interim code / macro-evaluation code you can
read in the individual dataset with this code:
  
```
data <- readData( dataNumber = 1, dataType = "Replicate")
```
  
then run the analysisCode function over this
  
```
EmaxCode(data)
```
  
Output from the `EmaxCode` function should be the micro-evaluation
dataset. However if this doesn’t return the right dataset it’s easier to
troubleshoot than running all 100 of the replicates…

You can check the macroCode` function by reading in the
micro-evaluation dataset:
  
```
data <- read.csv("./microevaluation/micro0001.csv")
```
  
and then run the `macroCode` function over this:
  
```
macroCode(data)
```
  
Once you’re happy with this, you can go on to look at how the
`interimCode` works by running
  
```
analyzeRep(analysisCode=emaxCode,
           interimCode=interimcode,
           replicate=1)
```
  
This will do the full analysis / micro-evaluation step at interims as
well as on the full dataset. **NOTE** that you need to specify interim
analysis proportions in the generated data BEFORE you analyze the
replicate dataset. If subjects are not assigned to interim cuts in the
generated data, the interim analysis steps will not be carried out (the
`analyzeData` function assumes that all subjects are in the FULL
dataset).
