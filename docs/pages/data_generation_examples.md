---
layout: page
title: Data Generation Examples
description: How to specify generateData arguments for various example cases
toc: true
---
  
## Examples:
  1. Basic linear model
  2. Basic Emax model
  3. Emax model with variability between replicates in parameters
  4. Emax model with correlation between replicates in parameters
  5. Emax model with interim analysis
  6. Binary data
  7. User-defined function for specifying mean effects for each treatment
  8. Crossover trial
  9. Generating more than one response outcome.
  

### Basic linear model
  
```
generateData( replicateN = 100, subjects=200, treatSubj = rep(100,2), 
treatDoses = c(0,100),  
genParNames = "ALPHA, BETA", genParMean = c(0,1), genParVCov=0,  
respEqn = "ALPHA+BETA*DOSE",  respVCov = 1,  
seed=12345)
``` 
  
Generates 100 replicates of 200 subjects split equally across the two
treatment arms. The treatment doses are 0 and 100. Parameters ALPHA and
BETA are generated where ALPHA=0 and BETA=1. The values of ALPHA and
BETA will not change between simulation replicates since `genParVCov = 0`. 
The linear predictor (`respEqn`) calculates the response for each treatment 
(dose) according to the equation `ALPHA+BETA*DOSE`, in this case the expected 
effect at each dose is the same as the dose level since ALPHA=0 and BETA=1. 
`respVCov` specifies the response variability / residual error which will be 
added to the expected values generated through `respEqn`. Here the residual
variability = 1 so values from a N(0,1) distribution will be generated
for each response and added to values from respEqn. The seed is set to
ensure reproducibility. By default MSToolkit assumes an additive error
structure, although this can be changed through settings in the 
`generateData(...)` call.

### Basic Emax Model
  
```
generateData( replicateN = 100, subjects=100, treatSubj = rep(20,5),
treatDoses = c(0, 5, 10, 50, 100),  
genParNames = "E0,ED50,EMAX", genParMean = c(0,50,100), genParVCov=diag(c(0,0,0)),  
respEqn = "E0 + ((DOSE * EMAX)/(DOSE + ED50))",  respVCov = 100)
```
  
This code produces 100 replicates of 100 subjects split equally across
the 5 treatment arms. The treatment arms are 0,5,10,50,100. Three
parameters are to be generated, and are named E0, ED50 and EMAX. These
take the values 0, 50 and 100 respectively. The variance-covariance
matrix is set to zero so again, these parameters will be the same across
all replicates. The linear predictor is the standard 3-parameter Emax
model. The variance of the response is set to 100 so responses will be
drawn from a Normal distribution with mean given by the Emax model and
variance 100 i.e. residual variability = 100.

### Emax model with variability between replicates in parameters
  
```
generateData( replicateN = 100, subjects=100, treatSubj = rep(20,5),
treatDoses = c(0, 5, 10, 50, 100),  
genParNames = "E0,ED50,EMAX", genParMean = c(0,50,100), genParVCov=diag(c(10,10,10)),  
respEqn = "E0 + ((DOSE * EMAX)/(DOSE + ED50))",  respVCov = 100)

```
  
Like example 2, this code produces 100 replicates of 100 subjects. In
this case the variance-covariance matrix is set to a diagonal matrix
with variance 10 for each parameter. This means that E0 ~ N(0,10); ED50
~ N(50,10) and EMAX ~ N(100,10) and the values for E0, ED50 and EMAX
for each replicate will be drawn from these Normal distributions. For
more explanation of why you might vary model parameter values between
replicates please read the [simulation overview](pages/simulation_overview.html).

### Emax model with correlation between replicates in parameters
  
```
generateData( replicateN = 100, subjects=100, treatSubj = rep(20,5),
treatDoses = c(0, 5, 10, 50, 100),  
genParNames = "E0,ED50,EMAX", genParMean = c(0,50,100), genParVCov=c(10,1,10,1,8,10),  
respEqn = "E0 + ((DOSE * EMAX)/(DOSE + ED50))",  respVCov = 100)
```
  
In this example we include correlations between the parameters E0, ED50,
EMAX. Each has variance = 10 but the correlation between ED50 and EMAX
is 0.8 - typical of what we see for this type of model. Note that the
variance-covariance matrix can be specified a number of ways - as an
array / matrix, or here as values of the lower triangle of that full
variance-covariance matrix. MSToolkit automatically converts these
numbers to a full covariance matrix and checks that it is
positive-semi-definite using the function parseCovMatrix.

### Emax model with interim analyses
  
```
generateData( replicateN = 2, subjects=100, treatSubj = rep(20,5),
treatDoses = c(0, 5, 10, 50, 100),  
genParNames = "E0,ED50,EMAX", genParMean = c(0,50,100), genParVCov=c(10,1,10,1,8,10),
respEqn = "E0 + ((DOSE * EMAX)/(DOSE + ED50))",  respVCov = 100,  
interimSubj=c(0.33,0.66))
```
  

In this example we include the argument interimSubj which allows us to
specify the proportion of subjects in each interim cut (note that the
proportions are cumulative). Thus we have our first interim after 1/3 of
data, the second at 2/3. Subjects are randomly assigned to interim
proportions 1,2,3 with probabilty 1/3 for each. This means that there
may not be ***exactly*** 1/3 of subjects assigned to each interim, as we
would experience in real life.

### Binary data
  
```
generateData( replicateN = 2, subjects=200, treatSubj = rep(100,2), 
treatDoses = c(0,100),  
genParNames = "ALPHA, BETA", genParMean = c(0,1), genParVCov=c(1,1), 
respEqn = "ALPHA+BETA*DOSE",  respDist="Binary")
```
  

In this example we simulate data from a binomial distribution. The
linear predictor is given by `log(p/1-p)=ALPHA+BETA*DOSE`, so that the
probability of a response = 1 increases with DOSE. Again, the values of
ALPHA and BETA vary between replicates. Count data can be similarly
generated by setting `respDist="Poisson"`. The canonical inverse link
functions are used to convert between the linear predictor and the input
to the response distribution i.e. for Binary data the link is `exp( x ) /
1 + exp( x )` while for Count data the link is `exp( x )`. Other link
functions can be specified by setting the argument `respInvLink` which
takes any R function. For example it is possible to generate binary data
from explicit probabilities by setting `respDist = "Binary"` and
`respInvLink = "NULL"` to use the identity link.

### User-defined function for specifying mean effects for each treatment
  
```
resp.fn<-function(data){
	RESP<-rep(0,nrow(data))
	RESP[data$TRT==1]<-data$MEAN1[data$TRT==1]
	RESP[data$TRT==2]<-data$MEAN2[data$TRT==2]
	RESP
	}
	
generateData( replicateN = 2, subjects=200, treatSubj = rep(100,2), 
treatDoses = c(0,1),  
genParNames = "MEAN1, MEAN2", genParMean = c(0,10), genParVCov=0,  
respEqn = resp.fn, respVCov=1)
```
  
Here we want to precisely what the mean response will be for two
different treatments (say an experimental drug and a control treatment).
We have written a function called `resp.fn` which calculates the mean
response for each subject which takes the data with treatment identifier
(TRT=1 or 2), DOSE information (here DOSE=0 or 1, but these are merely
labels and can be ignored in this example). The dataset also includes
two parameters - MEAN1 and MEAN2 which have values 0 and 10
respectively. `resp.fn` must take this dataset and return a value of RESP
for each subject. So the function initialises a value for RESP (=0) and
then for each treatment, assigns MEAN=1 where TRT=1 and MEAN=10 where
TRT=2.The resulting values then have residual error added (`respVCov=1`).

### Generating data from a crossover trial.
  
```
resp.fn<-function(data){
	RESP<-rep(0,nrow(data))
	RESP[data$DOSE==0]<-data$MEAN1[data$DOSE ==0]
	RESP[data$DOSE ==1]<-data$MEAN2[data$DOSE ==1]
	RESP
	}
	
generateData( replicateN = 2, subjects=20, treatSubj = rep(10,2), treatDoses = c(0,1),
treatType="Crossover", treatSeq=array(c(0,1,1,0),dim=c(2,2)),  
genParNames = "MEAN1, MEAN2", genParMean = c(0,10), genParVCov=0,  
genParBtwNames="MEAN1,MEAN2",genParBtwVCov=c(1,0.8,1),genParErrStruc="Additive",  
respEqn = resp.fn, respVCov=1)
```
  
Here we use the same function as above to generate the mean response for
each subject, however we have specified that we want to generate data
for a crossover trial. Note that here we swap from specifying RESP as a
function of TRT into RESP as a function of DOSE. This is because when we
specify a crossover trial, TRT becomes the treatment sequence that
subjects are allocated to, so DOSE becomes the treatment identifier for
each period. This could be a dose of drug, or simply a label (since the
means here are defined exactly for each treatment). We use the argument
`treatType` to show that we are generating data for a crossover trial and
we use `treatSeq` to define the treatment sequences. `treatSeq` should be an
array, but can specify any number of treatment periods and sequences
using the labels for treatments given by `treatDoses` - here 0 and 1. The
array has sequences as columns and periods as rows. Here we have
specified a 2x2 crossover with two sequences - DOSE=0 followed by DOSE=1
and vice versa. A three-period, two-treatment design could be specified
as `treatSeq=array(c(0,1,1,1,0,0),dim=c(3,2))`.

We must also take a little extra care in thinking about the data
generation processes in crossover trials. In a crossover we can assume
that the treatment effect is constant across treatment periods, but we
usually assume that an individual's responses between periods is
correlated - that is subjects with high observations in period 1 will
have high observations in period 2. We do this by specifying **between
subject variability** in the parameter means through `genParBtwNames`,
`genParBtwParVCov` and `genParErrStruc`. These options add between subject
variability to MEAN1 and MEAN2 with a correlation of 0.8 between them.
Thus subjects with high MEAN1 will also have high MEAN2. Finally the
residual error is added through `respVCov` as usual. Then, in the
analysis, we will look for treatment effects **within** subjects and we
can also look at period effects and sequence effects (none simulated
here).
