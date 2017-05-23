---
layout: page
title: generateData
---
## How it works

The [`generateData`](pages/generatedata.html)
function calls the low level generate data components to create sets of
simulated data. The following components are called to create aspects of
the simulated trial data:

-   `createTreatments(...)`: Creates a dataset of all possible
    treatment regimes to be allocated to subjects
-   `allocateTreatments(...)`: Allocates treatments to subjects in the
    simulated study
-   `createCovariates(...)`: Creates a set of fixed covariates for a
    simulated population
-   `createParameters(...)`: Creates simulated fixed and between
    subject parameters for subjects in each replicate
-   `createResponse(...)`: Creates a simulated response variable based
    on available derived data
-   `createMCAR(...)`: Adds a simulated "missing" flag to the data
-   `createDropout(...)`: Adds a simulated "missing" flag to the data
    based on a dropout function
-   `createInterims(...)`: Assigns subjects in the study to interim
    analyses
-   `createDirectories(...)`: creates ReplicateData directory under
    the current working directory.
-   `writeData(...)`: Which writes out the simulation replicate data
    in CSV<span class="Apple-converted-space"> </span>

The [`generateData`](pages/generatedata.html)
function iteratively builds and combines the data components for each
replicate, and stores the data in the "ReplicateData" subdirectory of
the working directory. This data can then be analyzed using a call to
the [`analyzeData`](pages/analyzedata.html) function.

## Arguments

The [`generateData`](pages/generatedata.html)
function takes a number of arguments which are passed down to the
various lower level functions.

### Required Arguments

| Argument Name | Description |
|--------------:|:------------|
| replicateN | Specifies how many replicates / simulated trials to generate |

| subjects   | TOTAL number of subjects for the whole design. The default behaviour is
to allocate subjects to each treatment with equal probability, which may
not guarantee equal allocation. See `treatSubj`
below for further details of treatment allocation methods. |

| treatDoses | Specifies the doses to be used in simulations. MSToolkit was designed to
evaluate the operating characteristics of clinical trials, but its
functionality can be extended to simulate non-clinical trials by
thinking of "doses" as other factors which vary between individuals
within a simulation replicate.  
In the generated dataset TRT defines the treatment arm to which a
subject is allocated and for parallel group designs this has a
corresponding, unique value of DOSE. However in the case of parallel
group trials, TRT is the treatment sequence to which a subject is
allocated and must have a corresponding treatment sequence. |

| respEqn   | Specifies the linear predictor for generating outcome values. This
should be a valid R expression or function. The expression can be
written directly in [`generateData`](pages/generatedata.html) or an R
function defined outside of [`generateData`](pages/generatedata.html) can be
called. This function must return a vector of equal length to the number
of rows in the generated data - one value per subject or one value per
observation (TIME) within each subject. |

### Optional Arguments
| Argument Name | Description |
|--------------:|:------------|
| treatSubj 
| treatProp     | `treatSubj` specifies the precise number of
subjects to allocate to each treatment (the sum of the elements of this
vector must equal `subjects` above). <span
class="s2">treatProp` is a vector of proportions specifying how
subjects are to be allocated to each treatment in the proportions
specified (the vector must be of the same length as the number of
treatments - the length of `treatDoses` above and
sum to 1). Specify only one of these arguments. 
`treatProp` defines the probabilities of
allocating each treatment and does not guarantee that the exact
proportion will be allocated to a given treatment. <span
class="s2">treatSubj` on the other hand allocates exactly the
specified number to each treatment. If the sum of the number of <span
class="s2">subjects` in treatSubj does not equal subjects (above)
then the sum of `treatSubj` is used in place of
`subjects`. |
| treatType
  treatSeq
  treatPeriod  | If `treatType` is "crossover" then `treatSeq` should contain the treatment sequences for
subjects to be allocated to. Each subject is then randomly allocated to
one of the treatment sequences unless `treatSubj`
is specified as above.  
`treatPeriod` defines the timing of observations /
response values. If any times are *less than zero* then it is assumed
that DOSE=0 for these measurements i.e. we assume a placebo run-in. For
times greater than or equal to zero DOSE is as specified in <span
class="s2">treatDoses`. |
| genParNames
  genParMean
  genParVCov
  genParNames   | defines the names to be used for the data generation model
parameters for calculations and in the output dataset. genParMean and
genParVCov define the mean value for these parameters and the
variance-covariance matrix defining how these parameters will vary
across trial replicates. By default we assume that genParVCov = 0 i.e.
parameters have fixed values across trial replicates. See the [Simulation overview](pages/simulation_overview.html) page for more information. |

| genParBtwNames
  genParBtwMean 
  genParBtwVCov 
  genParBtwCrit 
  genParErrStruc      | These parameters define how between subject variability is to be
included for the parameters used in respEqn. Variables defined in
genParNames which also appear in genParBtwNames will have values
generated from a (multivariate) Normal distribution with mean
genParBtwMean and variance-covariance matrix genParBtwVCov. By default
we assume that genParBtwMean = 0 for all parameters i.e. the parameters
used in respEqn will have means specified by genParMean (with between
replicate variability specified by genParVCov) and will vary between
subjects with covariance genParBtwVCov. This process mirrors the usual
hierarchical model construct with fixed and random effects.
genParBtwCrit applies ranges to the values generated (similar to
conCovCrit above). If genParErrStruc is specified as "additive" or
"proportional" then the subject specific variation is added to the fixed
effect values in an appropriate way. "additive" simply adds the values,
while "proportional" adds the subject specific variation to the logged
fixed effect value and then exponentiates. If genParErrStruc is "none"
then the two values are returned separately to the generated dataset for
the user to combine and use in an appropriate way. |

| respDist
  respVCov 
  respInvLink 
  respErrStruc 
  respCrit 
  respDigits  | These parameters define the distributional properties for the generated
response variable. respEqn gives the linear predictor for response,
defining how treatments, doses, covariates, time etc. relate to the mean
response for an individual. This linear predictor can then be used
within a normal distribution to define continuous response variables or,
with the appropriate link function (specified in respInvLink), can be
used with binomial or poisson distributions to create binary or count
data. If we are creating continuous response outcomes then we can
specify the residual (or within subject) variability, how this
variability is added to the values from respEqn through respErrStruc and
whether the generated residual values need to be constrained within
certain ranges (given by respCrit). Finally we can specify the number of
significant digits for the generated response. 
MSToolkit version 2.0 only uses 1 value for residual error, although
future versions will extend this to allow multiple residual error
parameters to be created. |

| interimSubj   |

interimSubj defines how subjects will be assigned to interim analysis
data subsets. This should be a vector of cumulative proportions e.g.
c(0.3,0.6) or c(0.25,0.5,0.75). MSToolkit will partition the dataset and
allocate subjects randomly to one of the interim analysis subsets. |

| mcarProp 
  mcarRule 
  dropFun  | These parameters define how missing data is to be generated and rules
for dropping subjects. dropFun can be any valid R function and so can
use dataset covariates, parameters and responses as drivers for the
dropout function.  |

| conCovNames 
  conCovMean 
  conCovVCov 
  conCovCrit 
  conCovMaxDraws  | These parameters define how continuous covariates are to be generated
across subjects within replicates. Values are drawn from (multivariate)
Normal distributions. conCovCrit specifies ranges or criteria for each
covariate value. If the number of draws from the distribution exceeds
conCovMaxDraws before an acceptable value is found then a warning is
given. |

| disCovNames 
  disCovVals 
  disCovProb 
  disCovProbArray  | These parameters define how discrete covariates are to be generated
across subjects within replicates. Values of the discrete parameters are
specified in disCovVals and then these values are generated in
proportions given by disCovProb or disCovProbArray if user wish to
specify associations between discrete covariate values. |

| extCovNames 
  extCovFile 
  extCovSubset 
  extCovRefCol 
  extCovSameRow 
  extCovDataId  | Covariate values can be sampled from an external file e.g. an existing
database in an ASCII file. These parameters define which variables to
sample from the external file, the name of that file and whether to
subset the data in that file before sampling. Users can choose to bring
into the generated dataset a reference variable identifying which rows
of the external datafile have been sampled (in order to check data
values). It is also possible to specify whether to sample covariate
values independently (default) or whether to sample covariate values
from within the same row of the external file, thus preserving
correlations between covariates without making normality assumptions. If
a value is given for extCovDataId then this is used to identify
covariate values from each unique ID within the external datafile.  |

