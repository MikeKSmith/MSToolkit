[**Data Generation:
Examples**](file:///Users/mike/Documents/SVN/www/data%20generation_%20examples.html)

1\. Basic linear model

2\. Basic Emax model

3\. Emax model with variability between replicates in parameters

4\. Emax model with correlation between replicates in parameters

5\. Emax model with interim analysis

6\. Binary data

7\. User-defined function for specifying mean effects for each treatment

8\. Crossover trial

9\. Generating more than one response outcome.

\

1.  **1. Basic linear model**

**generateData**<span class="s1">**(** </span>**replicateN**<span
class="s1"> **=** </span><span class="s2">**100**</span><span
class="s1">**,** </span>**subjects**<span class="s1">**=**</span><span
class="s2">**200**</span><span class="s1">**,**
</span>**treatSubj**<span class="s1"> **=** </span>**rep**<span
class="s1">**(**</span><span class="s2">**100**</span><span
class="s1">**,**</span><span class="s2">**2**</span><span
class="s1">**),<span class="Apple-converted-space"> </span>**</span>

**treatDoses**<span class="s1"> **=** </span>**c**<span
class="s1">**(**</span><span class="s2">**0**</span><span
class="s1">**,**</span><span class="s2">**100**</span><span
class="s1">**),<span class="Apple-converted-space"> </span>**</span>

**genParNames**<span class="s1"> **=** </span><span class="s3">**"ALPHA,
BETA"**</span><span class="s1">**,** </span>**genParMean**<span
class="s1"> **=** </span>**c**<span class="s1">**(**</span><span
class="s2">**0**</span><span class="s1">**,**</span><span
class="s2">**1**</span><span class="s1">**),**
</span>**genParVCov**<span class="s1">**=**</span><span
class="s2">**0**</span><span class="s1">**,**</span>

<span class="s4">**respEqn**</span><span class="s1"> **=**
</span>**"ALPHA+BETA\*DOSE"**<span class="s1">**,<span
class="Apple-converted-space">  </span>**</span><span
class="s4">**respVCov**</span><span class="s1"> **=** </span><span
class="s2">**1**</span><span class="s1">**,**</span>

<span class="s4">**seed**</span><span
class="s1">**=**</span>**12345**<span class="s1">**)**</span>

\

Generates 100 replicates of 200 subjects split equally across the two
treatment arms. The treatment doses are 0 and 100. Parameters ALPHA and
BETA are generated where ALPHA=0 and BETA=1. The values of ALPHA and
BETA will not change between simulation replicates since<span
class="s5"> genParVCov = 0</span>. The linear predictor (**respEqn**)
calculates the response for each treatment (dose) according to the
equation ALPHA+BETA\*DOSE, in this case the expected effect at each dose
is the same as the dose level since ALPHA=0 and BETA=1. respVCov
specifies the response variability / residual error which will be added
to the expected values generated through respEqn. Here the residual
variability = 1 so values from a N(0,1) distribution will be generated
for each response and added to values from respEqn. The seed is set to
ensure reproducability. By default MSToolkit assumes an additive error
structure, although this can be changed through settings in the [<span
class="s6">generateData(...)</span>](file:///Users/mike/Documents/SVN/www/generatedata(...).html)
call.

\

1.  **2. Basic Emax Model**

**generateData**<span class="s1">**(** </span>**replicateN**<span
class="s1"> **=** </span><span class="s2">**100**</span><span
class="s1">**,** </span>**subjects**<span class="s1">**=**</span><span
class="s2">**100**</span><span class="s1">**,**
</span>**treatSubj**<span class="s1"> **=** </span>**rep**<span
class="s1">**(**</span><span class="s2">**20**</span><span
class="s1">**,**</span><span class="s2">**5**</span><span
class="s1">**),**</span>

<span class="s4">**treatDoses**</span> **=** <span
class="s4">**c**</span>**(**<span class="s2">**0**</span>**,** <span
class="s2">**5**</span>**,** <span class="s2">**10**</span>**,** <span
class="s2">**50**</span>**,** <span class="s2">**100**</span>**),<span
class="Apple-converted-space"> </span>**

**genParNames**<span class="s1"> **=** </span><span
class="s3">**"E0,ED50,EMAX"**</span><span class="s1">**,**
</span>**genParMean**<span class="s1"> **=** </span>**c**<span
class="s1">**(**</span><span class="s2">**0**</span><span
class="s1">**,**</span><span class="s2">**50**</span><span
class="s1">**,**</span><span class="s2">**100**</span><span
class="s1">**),** </span>**genParVCov**<span
class="s1">**=**</span>**diag**<span class="s1">**(**</span>**c**<span
class="s1">**(**</span><span class="s2">**0**</span><span
class="s1">**,**</span><span class="s2">**0**</span><span
class="s1">**,**</span><span class="s2">**0**</span><span
class="s1">**)),**</span>

<span class="s4">**respEqn**</span><span class="s1"> **=**
</span>**"E0 + ((DOSE \* EMAX)/(DOSE + ED50))"**<span
class="s1">**,<span class="Apple-converted-space"> 
</span>**</span><span class="s4">**respVCov**</span><span class="s1">
**=** </span><span class="s2">**100**</span><span
class="s1">**)**</span>

\

This code produces 100 replicates of 100 subjects split equally across
the 5 treatment arms. The treatment arms are 0,5,10,50,100. Three
parameters are to be generated, and are named E0, ED50 and EMAX. These
take the values 0, 50 and 100 respectively. The variance-covariance
matrix is set to zero so again, these parameters will be the same across
all replicates. The linear predictor is the standard 3-parameter Emax
model. The variance of the response is set to 100 so responses will be
drawn from a Normal distribution with mean given by the Emax model and
variance 100 i.e. residual variability = 100.

\

1.  **3. Emax model with variability between replicates in parameters**

**generateData**<span class="s1">**(** </span>**replicateN**<span
class="s1"> **=** </span><span class="s2">**100**</span><span
class="s1">**,** </span>**subjects**<span class="s1">**=**</span><span
class="s2">**100**</span><span class="s1">**,**
</span>**treatSubj**<span class="s1"> **=** </span>**rep**<span
class="s1">**(**</span><span class="s2">**20**</span><span
class="s1">**,**</span><span class="s2">**5**</span><span
class="s1">**),**</span>

<span class="s4">**treatDoses**</span> **=** <span
class="s4">**c**</span>**(**<span class="s2">**0**</span>**,** <span
class="s2">**5**</span>**,** <span class="s2">**10**</span>**,** <span
class="s2">**50**</span>**,** <span class="s2">**100**</span>**),<span
class="Apple-converted-space"> </span>**

**genParNames**<span class="s1"> **=** </span><span
class="s3">**"E0,ED50,EMAX"**</span><span class="s1">**,**
</span>**genParMean**<span class="s1"> **=** </span>**c**<span
class="s1">**(**</span><span class="s2">**0**</span><span
class="s1">**,**</span><span class="s2">**50**</span><span
class="s1">**,**</span><span class="s2">**100**</span><span
class="s1">**),** </span>**genParVCov**<span
class="s1">**=**</span>**diag**<span class="s1">**(**</span>**c**<span
class="s1">**(**</span><span class="s2">**10**</span><span
class="s1">**,**</span><span class="s2">**10**</span><span
class="s1">**,**</span><span class="s2">**10**</span><span
class="s1">**)),**</span>

<span class="s4">**respEqn**</span><span class="s1"> **=**
</span>**"E0 + ((DOSE \* EMAX)/(DOSE + ED50))"**<span
class="s1">**,<span class="Apple-converted-space"> 
</span>**</span><span class="s4">**respVCov**</span><span class="s1">
**=** </span><span class="s2">**100**</span><span
class="s1">**)**</span>

\

Like example 2, this code produces 100 replicates of 100 subjects. In
this case the variance-covariance matrix is set to a diagonal matrix
with variance 10 for each parameter. This means that E0 \~ N(0,10); ED50
\~ N(50,10) and EMAX \~ N(100,10) and the values for E0, ED50 and EMAX
for each replicate will be drawn from these Normal distributions. For
more explanation of why you might vary model parameter values between
replicates please read the [<span class="s6">simulation
overview</span>](file:///Users/mike/Documents/SVN/www/simulation%20overview.html).

\

1.  **4. Emax model with correlation between replicates in parameters**

**generateData**<span class="s1">**(** </span>**replicateN**<span
class="s1"> **=** </span><span class="s2">**100**</span><span
class="s1">**,** </span>**subjects**<span class="s1">**=**</span><span
class="s2">**100**</span><span class="s1">**,**
</span>**treatSubj**<span class="s1"> **=** </span>**rep**<span
class="s1">**(**</span><span class="s2">**20**</span><span
class="s1">**,**</span><span class="s2">**5**</span><span
class="s1">**),**</span>

<span class="s4">**treatDoses**</span> **=** <span
class="s4">**c**</span>**(**<span class="s2">**0**</span>**,** <span
class="s2">**5**</span>**,** <span class="s2">**10**</span>**,** <span
class="s2">**50**</span>**,** <span class="s2">**100**</span>**),<span
class="Apple-converted-space"> </span>**

**genParNames**<span class="s1"> **=** </span><span
class="s3">**"E0,ED50,EMAX"**</span><span class="s1">**,**
</span>**genParMean**<span class="s1"> **=** </span>**c**<span
class="s1">**(**</span><span class="s2">**0**</span><span
class="s1">**,**</span><span class="s2">**50**</span><span
class="s1">**,**</span><span class="s2">**100**</span><span
class="s1">**),** </span>**genParVCov**<span
class="s1">**=**</span>**c**<span class="s1">**(**</span><span
class="s2">**10**</span><span class="s1">**,**</span><span
class="s2">**1**</span><span class="s1">**,**</span><span
class="s2">**10**</span><span class="s1">**,**</span><span
class="s2">**1**</span><span class="s1">**,**</span><span
class="s2">**8**</span><span class="s1">**,**</span><span
class="s2">**10**</span><span class="s1">**),**</span>

<span class="s4">**respEqn**</span><span class="s1"> **=**
</span>**"E0 + ((DOSE \* EMAX)/(DOSE + ED50))"**<span
class="s1">**,<span class="Apple-converted-space"> 
</span>**</span><span class="s4">**respVCov**</span><span class="s1">
**=** </span><span class="s2">**100**</span><span
class="s1">**)**</span>

\

In this example we include correlations between the parameters E0, ED50,
EMAX. Each has variance = 10 but the correlation between ED50 and EMAX
is 0.8 - typical of what we see for this type of model. Note that the
variance-covariance matrix can be specified a number of ways - as an
array / matrix, or here as values of the lower triangle of that full
variance-covariance matrix. MSToolkit automatically converts these
numbers to a full covariance matrix and checks that it is
positive-semi-definite using the function parseCovMatrix.

\

1.  **5. Emax model with interim analyses**

**generateData**<span class="s1">**(** </span>**replicateN**<span
class="s1"> **=** </span><span class="s2">**2**</span><span
class="s1">**,** </span>**subjects**<span class="s1">**=**</span><span
class="s2">**100**</span><span class="s1">**,**
</span>**treatSubj**<span class="s1"> **=** </span>**rep**<span
class="s1">**(**</span><span class="s2">**20**</span><span
class="s1">**,**</span><span class="s2">**5**</span><span
class="s1">**),**</span>

<span class="s4">**treatDoses**</span> **=** <span
class="s4">**c**</span>**(**<span class="s2">**0**</span>**,** <span
class="s2">**5**</span>**,** <span class="s2">**10**</span>**,** <span
class="s2">**50**</span>**,** <span class="s2">**100**</span>**),<span
class="Apple-converted-space"> </span>**

**genParNames**<span class="s1"> **=** </span><span
class="s3">**"E0,ED50,EMAX"**</span><span class="s1">**,**
</span>**genParMean**<span class="s1"> **=** </span>**c**<span
class="s1">**(**</span><span class="s2">**0**</span><span
class="s1">**,**</span><span class="s2">**50**</span><span
class="s1">**,**</span><span class="s2">**100**</span><span
class="s1">**),** </span>**genParVCov**<span
class="s1">**=**</span>**c**<span class="s1">**(**</span><span
class="s2">**10**</span><span class="s1">**,**</span><span
class="s2">**1**</span><span class="s1">**,**</span><span
class="s2">**10**</span><span class="s1">**,**</span><span
class="s2">**1**</span><span class="s1">**,**</span><span
class="s2">**8**</span><span class="s1">**,**</span><span
class="s2">**10**</span><span class="s1">**),**</span>

<span class="s4">**respEqn**</span><span class="s1"> **=**
</span>**"E0 + ((DOSE \* EMAX)/(DOSE + ED50))"**<span
class="s1">**,<span class="Apple-converted-space"> 
</span>**</span><span class="s4">**respVCov**</span><span class="s1">
**=** </span><span class="s2">**100**</span><span
class="s1">**,**</span>

**interimSubj**<span class="s1">**=**</span>**c**<span
class="s1">**(**</span><span class="s2">**0.33**</span><span
class="s1">**,**</span><span class="s2">**0.66**</span><span
class="s1">**))**</span>

\

In this example we include the argument interimSubj which allows us to
specify the proportion of subjects in each interim cut (note that the
proportions are cumulative). Thus we have our first interim after 1/3 of
data, the second at 2/3. Subjects are randomly assigned to interim
proportions 1,2,3 with probabilty 1/3 for each. This means that there
may not be ***exactly*** 1/3 of subjects assigned to each interim, as we
would experience in real life.

\

1.  **6. Binary data**

**generateData**<span class="s1">**(** </span>**replicateN**<span
class="s1"> **=** </span><span class="s2">**2**</span><span
class="s1">**,** </span>**subjects**<span class="s1">**=**</span><span
class="s2">**200**</span><span class="s1">**,**
</span>**treatSubj**<span class="s1"> **=** </span>**rep**<span
class="s1">**(**</span><span class="s2">**100**</span><span
class="s1">**,**</span><span class="s2">**2**</span><span
class="s1">**),<span class="Apple-converted-space"> </span>**</span>

**treatDoses**<span class="s1"> **=** </span>**c**<span
class="s1">**(**</span><span class="s2">**0**</span><span
class="s1">**,**</span><span class="s2">**100**</span><span
class="s1">**),<span class="Apple-converted-space"> </span>**</span>

**genParNames**<span class="s1"> **=** </span><span class="s3">**"ALPHA,
BETA"**</span><span class="s1">**,** </span>**genParMean**<span
class="s1"> **=** </span>**c**<span class="s1">**(**</span><span
class="s2">**0**</span><span class="s1">**,**</span><span
class="s2">**1**</span><span class="s1">**),**
</span>**genParVCov**<span class="s1">**=**</span>**c**<span
class="s1">**(**</span><span class="s2">**1**</span><span
class="s1">**,**</span><span class="s2">**1**</span><span
class="s1">**),**</span>

<span class="s4">**respEqn**</span><span class="s1"> **=**
</span>**"ALPHA+BETA\*DOSE"**<span class="s1">**,<span
class="Apple-converted-space">  </span>**</span><span
class="s4">**respDist**</span><span
class="s1">**=**</span>**"Binary"**<span class="s1">**,**</span>

<span class="s4">**seed**</span><span
class="s1">**=**</span>**12345**<span class="s1">**)**</span>

\

In this example we simulate data from a binomial distribution. The
linear predictor is given by log(p/1-p)=ALPHA+BETA\*DOSE, so that the
probability of a response = 1 increases with DOSE. Again, the values of
ALPHA and BETA vary between replicates. Count data can be similarly
generated by setting respDist="Poisson". The canonical inverse link
functions are used to convert between the linear predictor and the input
to the response distribution i.e. for Binary data the link is exp( x ) /
1 + exp( x ) while for Count data the link is exp( x ). Other link
functions can be specified by setting the argument respInvLink which
takes any R function. For example it is possible to generate binary data
from explicit probabilities by setting respDist = "Binary" and
respInvLink = "NULL" to use the identity link.

\

1.  **7. User-defined function for specifying mean effects for each
    treatment**

**resp.fn**<span class="s1">**\<-**</span><span
class="s7">**function**</span><span class="s1">**(**</span>**data**<span
class="s1">**){**</span>

<span class="s1">**<span class="Apple-tab-span">
</span>**</span>**RESP**<span class="s1">**\<-**</span>**rep**<span
class="s1">**(**</span><span class="s2">**0**</span><span
class="s1">**,**</span>**nrow**<span
class="s1">**(**</span>**data**<span class="s1">**))**</span>

<span class="s1">**<span class="Apple-tab-span">
</span>**</span>**RESP**<span class="s1">**[**</span>**data**<span
class="s1">**\$**</span>**TRT**<span class="s1">**==**</span><span
class="s2">**1**</span><span class="s1">**]\<-**</span>**data**<span
class="s1">**\$**</span>**MEAN1**<span
class="s1">**[**</span>**data**<span
class="s1">**\$**</span>**TRT**<span class="s1">**==**</span><span
class="s2">**1**</span><span class="s1">**]**</span>

<span class="s1">**<span class="Apple-tab-span">
</span>**</span>**RESP**<span class="s1">**[**</span>**data**<span
class="s1">**\$**</span>**TRT**<span class="s1">**==**</span><span
class="s2">**2**</span><span class="s1">**]\<-**</span>**data**<span
class="s1">**\$**</span>**MEAN2**<span
class="s1">**[**</span>**data**<span
class="s1">**\$**</span>**TRT**<span class="s1">**==**</span><span
class="s2">**2**</span><span class="s1">**]**</span>

<span class="s1">**<span class="Apple-tab-span">
</span>**</span>**RESP**

**<span class="Apple-tab-span"> </span>}**

**generateData**<span class="s1">**(** </span>**replicateN**<span
class="s1"> **=** </span><span class="s2">**2**</span><span
class="s1">**,** </span>**subjects**<span class="s1">**=**</span><span
class="s2">**200**</span><span class="s1">**,**
</span>**treatSubj**<span class="s1"> **=** </span>**rep**<span
class="s1">**(**</span><span class="s2">**100**</span><span
class="s1">**,**</span><span class="s2">**2**</span><span
class="s1">**),<span class="Apple-converted-space"> </span>**</span>

**treatDoses**<span class="s1"> **=** </span>**c**<span
class="s1">**(**</span><span class="s2">**0**</span><span
class="s1">**,**</span><span class="s2">**1**</span><span
class="s1">**),<span class="Apple-converted-space"> </span>**</span>

**genParNames**<span class="s1"> **=** </span><span class="s3">**"MEAN1,
MEAN2"**</span><span class="s1">**,** </span>**genParMean**<span
class="s1"> **=** </span>**c**<span class="s1">**(**</span><span
class="s2">**0**</span><span class="s1">**,**</span><span
class="s2">**10**</span><span class="s1">**),**
</span>**genParVCov**<span class="s1">**=**</span><span
class="s2">**0**</span><span class="s1">**,**</span>

**respEqn**<span class="s1"> **=** </span>**resp.fn**<span
class="s1">**,** </span>**respVCov**<span class="s1">**=**</span><span
class="s2">**1**</span><span class="s1">**,**</span>

<span class="s4">**seed**</span><span
class="s1">**=**</span>**12345**<span class="s1">**)**</span>

\

Here we want to precisely what the mean response will be for two
different treatments (say an experimental drug and a control treatment).
We have written a function called resp.fn which calculates the mean
response for each subject which takes the data with treatment identifier
(TRT=1 or 2), DOSE information (here DOSE=0 or 1, but these are merely
labels and can be ignored in this example). The dataset also includes
two parameters - MEAN1 and MEAN2 which have values 0 and 10
respectively. resp.fn must take this dataset and return a value of RESP
for each subject. So the function initialises a value for RESP (=0) and
then for each treatment, assigns MEAN=1 where TRT=1 and MEAN=10 where
TRT=2.<span class="Apple-converted-space">  </span>The resulting values
then have residual error add (respVCov=1).

\

\

1.  **8. Generating data from a crossover trial.**

**resp.fn**<span class="s1">**\<-**</span><span
class="s7">**function**</span><span class="s1">**(**</span>**data**<span
class="s1">**){**</span>

<span class="s1">**<span class="Apple-tab-span">
</span>**</span>**RESP**<span class="s1">**\<-**</span>**rep**<span
class="s1">**(**</span><span class="s2">**0**</span><span
class="s1">**,**</span>**nrow**<span
class="s1">**(**</span>**data**<span class="s1">**))**</span>

<span class="s1">**<span class="Apple-tab-span">
</span>**</span>**RESP**<span class="s1">**[**</span>**data**<span
class="s1">**\$**</span>**DOSE**<span class="s1">**==**</span><span
class="s2">**0**</span><span class="s1">**]\<-**</span>**data**<span
class="s1">**\$**</span>**MEAN1**<span
class="s1">**[**</span>**data**<span class="s1">**\$**</span>**DOSE**
<span class="s1">**==**</span><span class="s2">**0**</span><span
class="s1">**]**</span>

<span class="s1">**<span class="Apple-tab-span">
</span>**</span>**RESP**<span class="s1">**[**</span>**data**<span
class="s1">**\$**</span>**DOSE** <span class="s1">**==**</span><span
class="s2">**1**</span><span class="s1">**]\<-**</span>**data**<span
class="s1">**\$**</span>**MEAN2**<span
class="s1">**[**</span>**data**<span class="s1">**\$**</span>**DOSE**
<span class="s1">**==**</span><span class="s2">**1**</span><span
class="s1">**]**</span>

<span class="s1">**<span class="Apple-tab-span">
</span>**</span>**RESP**

**<span class="Apple-tab-span"> </span>}**

**generateData**<span class="s1">**(** </span>**replicateN**<span
class="s1"> **=** </span><span class="s2">**2**</span><span
class="s1">**,** </span>**subjects**<span class="s1">**=**</span><span
class="s2">**20**</span><span class="s1">**,** </span>**treatSubj**<span
class="s1"> **=** </span>**rep**<span class="s1">**(**</span><span
class="s2">**10**</span><span class="s1">**,**</span><span
class="s2">**2**</span><span class="s1">**),**
</span>**treatDoses**<span class="s1"> **=** </span>**c**<span
class="s1">**(**</span><span class="s2">**0**</span><span
class="s1">**,**</span><span class="s2">**1**</span><span
class="s1">**),**</span>

**treatType**<span class="s1">**=**</span><span
class="s3">**"Crossover"**</span><span class="s1">**,**
</span>**treatSeq**<span class="s1">**=**</span>**array**<span
class="s1">**(**</span>**c**<span class="s1">**(**</span><span
class="s2">**0**</span><span class="s1">**,**</span><span
class="s2">**1**</span><span class="s1">**,**</span><span
class="s2">**1**</span><span class="s1">**,**</span><span
class="s2">**0**</span><span class="s1">**),**</span>**dim**<span
class="s1">**=**</span>**c**<span class="s1">**(**</span><span
class="s2">**2**</span><span class="s1">**,**</span><span
class="s2">**2**</span><span class="s1">**)),**</span>

**genParNames**<span class="s1"> **=** </span><span class="s3">**"MEAN1,
MEAN2"**</span><span class="s1">**,** </span>**genParMean**<span
class="s1"> **=** </span>**c**<span class="s1">**(**</span><span
class="s2">**0**</span><span class="s1">**,**</span><span
class="s2">**10**</span><span class="s1">**),**
</span>**genParVCov**<span class="s1">**=**</span><span
class="s2">**0**</span><span class="s1">**,**</span>

**genParBtwNames**<span class="s1">**=**</span><span
class="s3">**"MEAN1,MEAN2"**</span><span
class="s1">**,**</span>**genParBtwVCov**<span
class="s1">**=**</span>**c**<span class="s1">**(**</span><span
class="s2">**1**</span><span class="s1">**,0.8,**</span><span
class="s2">**1**</span><span
class="s1">**),**</span>**genParErrStruc**<span
class="s1">**=**</span><span class="s3">**"Additive"**</span><span
class="s1">**,**</span>

**respEqn**<span class="s1"> **=** </span>**resp.fn**<span
class="s1">**,** </span>**respVCov**<span class="s1">**=**</span><span
class="s2">**1**</span><span class="s1">**,**</span>

<span class="s4">**seed**</span><span
class="s1">**=**</span>**12345**<span class="s1">**)**</span>

\

Here we use the same function as above to generate the mean response for
each subject, however we have specified that we want to generate data
for a crossover trial. Note that here we swap from specifying RESP as a
function of TRT into RESP as a function of DOSE. This is because when we
specify a crossover trial, TRT becomes the treatment sequence that
subjects are allocated to, so DOSE becomes the treatment identifier for
each period. This could be a dose of drug, or simply a label (since the
means here are defined exactly for each treatment). We use the argument
treatType to show that we are generating data for a crossover trial and
we use treatSeq to define the treatment sequences. treatSeq should be an
array, but can specify any number of treatment periods and sequences
using the labels for treatments given by treatDoses - here 0 and 1. The
array has sequences as columns and periods as rows. Here we have
specified a 2x2 crossover with two sequences - DOSE=0 followed by DOSE=1
and vice versa. A three-period, two-treatment design could be specified
as treatSeq<span class="s1">=</span>array<span class="s1">(</span>c<span
class="s1">(</span><span class="s2">0</span><span
class="s1">,</span><span class="s2">1</span><span
class="s1">,</span><span class="s2">1</span><span
class="s1">,</span><span class="s2">1</span><span
class="s1">,</span><span class="s2">0</span><span
class="s1">,</span><span class="s2">0</span><span
class="s1">),</span>dim<span class="s1">=</span>c<span
class="s1">(</span><span class="s2">3</span><span
class="s1">,</span><span class="s2">2</span><span
class="s1">))</span><span class="s8">**.**</span>

\

We must also take a little extra care in thinking about the data
generation processes in crossover trials. In a crossover we can assume
that the treatment effect is constant across treatment periods, but we
usually assume that an individual's responses between periods is
correlated - that is subjects with high observations in period 1 will
have high observations in period 2. We do this by specifying **between
subject variability** in the parameter means through genParBtwNames,
genParBtwParVCov and genParErrStruc. These options add between subject
variability to MEAN1 and MEAN2 with a correlation of 0.8 between them.
Thus subjects with high MEAN1 will also have high MEAN2. Finally the
residual error is added through respVCov as usual. Then, in the
analysis, we will look for treatment effects **within** subjects and we
can also look at period effects and sequence effects (none simulated
here).

\

1.  **9. Generating more than one response outcome.**<span
    class="Apple-converted-space">  </span><span class="s9">(e.g.
    Generating efficacy and safety data).</span>

This can be achieved by using two generateData steps with two separate
working directories (use setwd(...) to set the working directory in R).
When generating parameters for the efficacy and safety models, it may be
advantageous to generate the full set of parameters for both models
within each generateData step i.e. generate safety parameters within the
efficacy generateData step and vice versa. If the same seed is used in
both steps then the parameters across the two generateData steps will
have the same parameter values. Merging the replicate000x.csv datasets
together will then create data with the two response variables properly
correlated. Below is some example code for this:

\

**\#\#\# Set the working directory to point to the “Efficacy” directory
(must already exist).**

<span class="s4">**setwd**</span><span
class="s10">**(**</span>**“../efficacy”**<span class="s10">**)**</span>

**generateData**<span class="s10">**(**</span>**rep**<span
class="s10">**=**</span><span class="s11">**2**</span><span
class="s10">**,**</span>**subj**<span class="s10">**=**</span><span
class="s11">**10**</span><span
class="s10">**,**</span>**treatDoses**<span
class="s10">**=**</span>**c**<span class="s10">**(**</span><span
class="s11">**0**</span><span class="s10">**,**</span><span
class="s11">**10**</span><span
class="s10">**),**</span>**genParNames**<span
class="s10">**=**</span><span
class="s12">**"EFFMEAN,SAFEMEAN"**</span><span class="s10">**,**
</span>**genParMean**<span class="s10">**=**</span>**c**<span
class="s10">**(**</span><span class="s11">**10**</span><span
class="s10">**,**</span><span class="s11">**20**</span><span
class="s10">**),**</span>

**genParVCov**<span class="s10">**=**</span>**c**<span
class="s10">**(**</span><span class="s11">**0**</span><span
class="s10">**,**</span><span class="s11">**1**</span><span
class="s10">**),**</span>**respEqn**<span class="s10">**=**</span><span
class="s12">**"EFFMEAN"**</span><span
class="s10">**,**</span>**seed**<span class="s10">**=**</span><span
class="s11">**123**</span><span class="s10">**)**</span>

\

<span class="s4">**setwd**</span><span
class="s10">**(**</span>**"../safety"**<span class="s10">**)**</span>

**generateData**<span class="s10">**(**</span>**rep**<span
class="s10">**=**</span><span class="s11">**2**</span><span
class="s10">**,**</span>**subj**<span class="s10">**=**</span><span
class="s11">**10**</span><span
class="s10">**,**</span>**treatDoses**<span
class="s10">**=**</span>**c**<span class="s10">**(**</span><span
class="s11">**0**</span><span class="s10">**,**</span><span
class="s11">**10**</span><span
class="s10">**),**</span>**genParNames**<span
class="s10">**=**</span><span
class="s12">**"EFFMEAN,SAFEMEAN"**</span><span class="s10">**,**
</span>**genParMean**<span class="s10">**=**</span>**c**<span
class="s10">**(**</span><span class="s11">**10**</span><span
class="s10">**,**</span><span class="s11">**20**</span><span
class="s10">**),**</span>

**genParVCov**<span class="s10">**=**</span>**c**<span
class="s10">**(**</span><span class="s11">**0**</span><span
class="s10">**,**</span><span class="s11">**1**</span><span
class="s10">**),**</span>**respEqn**<span class="s10">**=**</span><span
class="s12">**"SAFEMEAN"**</span><span
class="s10">**,**</span>**seed**<span class="s10">**=**</span><span
class="s11">**123**</span><span class="s10">**)**</span>

\

<span class="s4">**eff**</span><span class="s10">**\<-**</span><span
class="s4">**readAllData**</span><span
class="s10">**(**</span>**"ReplicateData"**<span
class="s10">**,**</span><span class="s4">**workingPath**</span><span
class="s10">**=**</span>**"../efficacy"**<span class="s10">**)**</span>

**eff**<span class="s10">**\$**</span>**EFF**<span
class="s10">**\<-**</span>**eff**<span class="s10">**\$**</span>**RESP**

<span class="s4">**eff**</span><span class="s10">**\<-**</span><span
class="s4">**eff**</span><span class="s10">**[,**</span><span
class="s4">**c**</span><span
class="s10">**(**</span>**"Replicate"**<span
class="s10">**,**</span>**"SUBJ"**<span
class="s10">**,**</span>**"TRT"**<span
class="s10">**,**</span>**"DOSE"**<span
class="s10">**,**</span>**"EFFMEAN"**<span
class="s10">**,**</span>**"SAFEMEAN"**<span
class="s10">**,**</span>**"EFF"**<span class="s10">**)]**</span>

\

**safety**<span class="s10">**\<-**</span>**readAllData**<span
class="s10">**(**</span><span
class="s12">**"ReplicateData"**</span><span
class="s10">**,**</span>**workingPath**<span
class="s10">**=**</span><span class="s12">**"../safety"**</span><span
class="s10">**)**</span>

**safety**<span class="s10">**\$**</span>**SAFETY**<span
class="s10">**\<-**</span>**safety**<span
class="s10">**\$**</span>**RESP**

<span class="s4">**safety**</span><span class="s10">**\<-**</span><span
class="s4">**safety**</span><span class="s10">**[,**</span><span
class="s4">**c**</span><span
class="s10">**(**</span>**"Replicate"**<span
class="s10">**,**</span>**"SUBJ"**<span
class="s10">**,**</span>**"TRT"**<span
class="s10">**,**</span>**"DOSE"**<span
class="s10">**,**</span>**"EFFMEAN"**<span
class="s10">**,**</span>**"SAFEMEAN"**<span
class="s10">**,**</span>**"SAFETY"**<span class="s10">**)]**</span>

**data**<span class="s10">**\<-**</span>**merge**<span
class="s10">**(**</span>**eff**<span
class="s10">**,**</span>**safety**<span class="s10">**)**</span>

\

**writeData**<span class="s10">**(**</span>**data**<span
class="s10">**,**</span>**max**<span
class="s10">**(**</span>**data**<span
class="s10">**\$**</span>**Replicate**<span
class="s10">**),**</span>**dataType**<span class="s10">**=**</span><span
class="s12">**"ReplicateData"**</span><span
class="s10">**,**</span>**workingPath**<span
class="s10">**=**</span><span class="s12">**"../"**</span><span
class="s10">**)**</span>
