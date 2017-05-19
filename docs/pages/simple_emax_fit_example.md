<span class="s1">[**Analysis: Simple Emax Fit
Example**](analysis_%20simple%20emax%20fit%20example.html)</span>

**Data generation**

This code produces 5 replicates of 100 subjects. In this case the
variance-covariance matrix is set to a diagonal matrix with variance 0.5
for E0, 30 for ED50 and 10 for EMAX. This means that E0 \~ N(2,0.3);
ED50 \~ N(50,30) and EMAX \~ N(10,10) and the values for E0, ED50 and
EMAX for each replicate will be drawn from these Normal distributions.
For more explanation of why you might vary model parameter values
between replicates please read the [<span class="s1">simulation
overview</span>](simulation%20overview.html). Residual error is set to
draw values from a N(0,2) distribution which will be added to the
individual responses for each subject. Two interim analyses are set, one
at 30% and one at 70% of the data.

generateData<span class="s2">( </span>replicateN<span class="s2"> =
</span><span class="s3">5</span><span class="s2">, </span>subjects<span
class="s2"> = </span><span class="s3">100</span><span class="s2">,
</span>treatDoses<span class="s2"> = </span>c<span
class="s2">(</span><span class="s3">0</span><span class="s2">,
</span><span class="s3">5</span><span class="s2">, </span><span
class="s3">10</span><span class="s2">, </span><span
class="s3">50</span><span class="s2">, </span><span
class="s3">100</span><span class="s2">),<span
class="Apple-converted-space"> </span></span>

<span class="s2"><span class="Apple-converted-space"> 
</span></span>genParNames<span class="s2"> = </span><span
class="s4">"E0,ED50,EMAX"</span><span class="s2">,
</span>genParMean<span class="s2"> = </span>c<span
class="s2">(</span><span class="s3">2</span><span
class="s2">,</span><span class="s3">50</span><span
class="s2">,</span><span class="s3">10</span><span class="s2">),
</span>genParVCov<span class="s2"> = </span>c<span
class="s2">(</span>.5<span class="s2">,</span><span
class="s3">30</span><span class="s2">,</span><span
class="s3">10</span><span class="s2">),<span
class="Apple-converted-space"> </span></span>

<span class="s2"><span class="Apple-converted-space"> 
</span></span><span class="s5">respEqn</span><span class="s2"> =
</span>"E0 + ((DOSE \* EMAX)/(DOSE + ED50))"<span class="s2">,<span
class="Apple-converted-space">  </span></span><span
class="s5">respVCov</span><span class="s2"> = </span><span
class="s3">2</span><span class="s2">,<span
class="Apple-converted-space"> </span></span>

<span class="s2"><span class="Apple-converted-space"> 
</span></span>interimSubj<span class="s2"> = </span><span
class="s4">".3,.7"</span><span class="s2"><span
class="Apple-converted-space"> </span></span>

<span class="Apple-converted-space">  </span>)

\

**Analysis & Micro-evaluation**

Full code for data generation and analysis of this example can be viewed
on the [<span class="s1">emax.R</span>](emax.r.html) page.

NOTE: **REQUIRED** values returned from analysisCode / micro-evaluation:
**MEAN**, **SE**, **LOWER**, **UPPER**, **N** calculated for each
**DOSE**.

emaxCode<span class="s6"> \<- </span><span
class="s7">function</span><span class="s6">(</span>data<span
class="s6">){</span>

<span class="s6"><span class="Apple-converted-space">   
</span></span>uniDoses<span class="s6"> \<- </span>sort<span
class="s6">( </span>unique<span class="s6">(</span>data<span
class="s6">\$</span>DOSE<span class="s6">))<span
class="Apple-converted-space"> </span></span>

<span class="s6"><span class="Apple-tab-span">
</span></span>obsMean<span class="s6"> \<- </span>tapply<span
class="s6">(</span>data<span class="s6">\$</span>RESP<span class="s6">,
</span>list<span class="s6">(</span>data<span
class="s6">\$</span>DOSE<span class="s6">), </span>mean<span
class="s6">)</span>

<span class="Apple-tab-span"> </span><span class="s5">obsSD</span> \<-
<span class="s5">tapply</span>(<span class="s5">data</span>\$<span
class="s5">RESP</span>, <span class="s5">list</span>(<span
class="s5">data</span>\$<span class="s5">DOSE</span>), <span
class="s5">sd</span>)<span class="Apple-converted-space">               
                                                   </span>

<span class="s6"><span class="Apple-converted-space">   
</span></span>eFit<span class="s6"> \<- </span>emax.fit<span
class="s6">( </span>data<span class="s6">\$</span>RESP<span class="s6">,
</span>data<span class="s6">\$</span>DOSE<span class="s6"> )</span>

<span class="s6"><span class="Apple-converted-space">   
</span></span>outDf<span class="s6"> \<- </span>data.frame<span
class="s6">( </span>DOSE<span class="s6"> = </span>uniDoses<span
class="s6">,<span class="Apple-converted-space"> </span></span>

<span class="s6"><span class="Apple-converted-space">     
</span></span>MEAN<span class="s6"> = </span>eFit<span
class="s6">\$</span>fitpred<span class="s6">,<span
class="Apple-converted-space"> </span></span>

<span class="s6"><span class="Apple-converted-space">     
</span></span>SE<span class="s6"> = </span>eFit<span
class="s6">\$</span>sdpred<span class="s6">,</span>

<span class="s6"><span class="Apple-tab-span"> </span><span
class="Apple-converted-space">  </span></span>SDDIF<span class="s6"> =
</span>eFit<span class="s6">\$</span>sddif<span class="s6">)</span>

<span class="s6"><span class="Apple-converted-space">   
</span></span>outDf<span class="s6">\$</span>LOWER<span class="s6"> \<-
</span>outDf<span class="s6">\$</span>MEAN<span class="s6"> -
</span><span class="s8">1.96</span><span class="s6">\*</span>outDf<span
class="s6">\$</span>SE

<span class="s6"><span class="Apple-converted-space">   
</span></span>outDf<span class="s6">\$</span>UPPER<span class="s6"> \<-
</span>outDf<span class="s6">\$</span>MEAN<span class="s6"> +
</span><span class="s8">1.96</span><span class="s6">\*</span>outDf<span
class="s6">\$</span>SE

<span class="s6"><span class="Apple-converted-space">   
</span></span>outDf<span class="s6">\$</span>N<span class="s6"> <span
class="Apple-converted-space">    </span>\<- </span>table<span
class="s6">(</span>data<span class="s6">\$</span>DOSE<span
class="s6">)</span>

<span class="s6"><span class="Apple-tab-span"> </span></span>outDf<span
class="s6">\$</span>OBSMEAN<span class="s6"> \<- </span>obsMean

<span class="s6"><span class="Apple-tab-span"> </span></span>outDf<span
class="s6">\$</span>OBSSD<span class="s6"> \<- </span>obsSD

<span class="Apple-converted-space">    </span><span
class="s5">outDf</span><span class="Apple-converted-space"> </span>

<span class="s9">}<span class="Apple-converted-space"> 
</span></span><span class="Apple-converted-space">                     
                                                                       
                   </span>

<span class="Apple-converted-space">             </span>

The example code above is using the function **emax.fit**(...) to
perform the basic analysis. This function allows access to many
summaries of the model that is fit to the data, and so, quickly conforms
to the pre-specified output for micro-evaluation (mean, std.error, lower
and upper interval estimate). The replicate datasets are passed to the
function through the argument data. The replicate datasets conform to
the standard formats if we have used [<span
class="s1">**generateData**(...)</span>](generatedata(...).html) so we
should expect the response variable to be called RESP and for the dose
variable to be called DOSE. If we have specified covariates in the
[<span class="s1">**generateData**(...)</span>](generatedata(...).html)
step then these will be present in the replicate dataset. **NOTE** that
R is case-sensitive so covariate names will also be case sensitive.<span
class="Apple-converted-space"> </span>

To practice building the analysisCode, we can import one replicate
dataset

<span class="s9">data \<- readData( dataNumber = 1, dataType =
"Replicate)</span> and then we can build our analysis code or test out
functionality.

Analysis output (**micro000x**.csv micro-evaluation datasets) are stored
in the **MicroEvaluation** sub-directory within the working directory.
One dataset is stored for each replicate. The [<span
class="s1">**analyzeData**(...)</span>](analyzedata(...).html) function
compiles these into a single **MicroSummary**.csv data file after the
last replicate is analyzed. This **MicroSummary**.csv file is stored at
the top level of the working directory. it contains dose-wise means,
standard errors, lower and upper confidence limits and any other output
specified in the **analysisCode**.

\

**Macro-evaluation**

macroCode<span class="s2"> \<- </span><span
class="s10">function</span><span class="s2">(</span>data<span
class="s2">) {</span>

<span class="s2"><span class="Apple-converted-space">  </span></span>\#
Is effect at highest dose significant?

<span class="s2"><span class="Apple-converted-space"> 
</span></span>success<span class="s2"> \<- </span>data<span
class="s2">\$</span>LOWER<span class="s2">[</span>data<span
class="s2">\$</span>INTERIM<span class="s2">==</span>max<span
class="s2">(</span>data<span class="s2">\$</span>INTERIM<span
class="s2">) & </span>data<span class="s2">\$</span>DOSE<span
class="s2">==</span>max<span class="s2">(</span>data<span
class="s2">\$</span>DOSE<span class="s2">)] \> </span><span
class="s3">7</span>

<span class="s2"><span class="Apple-converted-space"> 
</span></span>data.frame<span class="s2">( </span>SUCCESS<span
class="s2"> = </span>success<span class="s2"> )</span>

}

<span class="Apple-converted-space">  </span>

The above macro-evaluation is used to assess trial-level success or
failure. This code looks at the lower confidence limits of the
difference over placebo and if all of these values are less than zero
declares the trial a failure. If at least ONE of these confidence limits
are above zero then the trial is deemed a success. The
**MacroEvaluation** directory contains the results of macro-evaluation
(**macroCode** output) for each replicate in the **macro000x**.csv
dataset. The [<span
class="s1">**analyzeData**(...)</span>](analyzedata(...).html) function
compiles these into a **MacroSummary**.csv file which is stored at the
top level of the working directory. The **macroCode** function can
return any macro-evaluation summary that is useful for assessing the
trial-level operating characteristics e.g. Success/Failure, bias and
precision of parameter estimates, etc. These will be compiled into the
**MacroSummary**.csv file and can then be summarised across replicates
by reading in the **MacroSummary**.csv dataset. Further summaries of the
operating characteristics have not been prespecified since these depend
on the operating characteristic of interest.

\

**Interim Code**

interimCode<span class="s2"> \<- </span><span
class="s10">function</span><span class="s2">( </span>data<span
class="s2"> ){</span>

<span class="s2"><span class="Apple-converted-space">  </span></span>\#
DROP any doses where the lower bound of the difference from placebo is
negative

<span class="s2"><span class="Apple-converted-space"> 
</span></span>dropdose<span class="s2"><span
class="Apple-converted-space">  </span>\<- </span>with<span class="s2">(
</span>data<span class="s2"> , </span>DOSE<span class="s2"> [
</span>LOWER<span class="s2"> \< </span><span class="s3">0</span><span
class="s2"> & </span>DOSE<span class="s2"> != </span><span
class="s3">0</span><span class="s2">] )</span>

<span class="s2"><span class="Apple-converted-space"> 
</span></span>outList<span class="s2"> \<- </span>list<span
class="s2">()</span>

<span class="s2"><span class="Apple-converted-space"> 
</span></span><span class="s10">if</span><span class="s2">(
</span>length<span class="s2">(</span>dropdose<span class="s2">) \>
</span><span class="s3">0</span><span class="s2"> ) </span>outList<span
class="s2">\$</span>DROP<span class="s2"> \<- </span>dropdose

<span class="s2"><span class="Apple-converted-space"> 
</span></span>outList<span class="s2">\$</span>STOP<span class="s2"> \<-
</span>length<span class="s2">(</span>dropdose<span class="s2">) ==
</span>nrow<span class="s2">(</span>data<span class="s2">)-</span><span
class="s3">1</span>

<span class="s2"><span class="Apple-converted-space"> 
</span></span>outList

}

\

**NOTE** that you need to specify interim analysis proportions in the
generated data BEFORE you analyze the replicate dataset. If subjects are
not assigned to interim cuts in the generated data, the interim analysis
steps will not be carried out (the analyzeData function assumes that all
subjects are in the FULL dataset).

The **interimcode** function above will be run after the
**analysisCode** at each specified interim analysis and the function
specifies rules for dropping doses or terminating the study.
**REQUIRED** outputs are **DROP** and **STOP**. The code above checks
whether the lower confidence limit for the difference from placebo for
each dose is below zero i.e. no difference from placebo. The DROP
variable is a vector of those doses which are to be dropped from the
study. STOP is a flag which indicates whether the study should stop. In
this case, if all doses are included in DROP (not including placebo)
then the study is stopped.

The micro-evaluation dataset **micro000x**.csv found in the
**MicroEvaluation** sub-directory compiles together results from the
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

\

**Running analysis using** [<span
class="s1">**analyzeData(...)**</span>](analyzedata(...).html)

analyzeData<span class="s6">(</span>analysisCode<span class="s6"> =
</span>emaxCode<span class="s6">, </span>macroCode<span class="s6"> =
</span>macroCode<span class="s6">, </span>interimCode<span class="s6"> =
</span>interimCode<span class="s6"> )</span>

\

**analysisCode** and **macroCode** are **REQUIRED** inputs.

<span class="s1">[**analyzeData**<span
class="s11">(...)</span>](analyzedata(...).html)</span> wraps the
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
to accept a working input dataset called **INFILE** and should return a
final dataset of results called **OUTFILE**. Both of these datasets
should be contained in the **WORK** directory (i.e. **NOT** permanent
SAS datasets). Code should be written to be robust to errors. It is the
user's responsibility to track errors within the SAS code. The
analyzeData function will call the SAS code and expeect the OUTFILE
return dataset with the REQUIRED variables described above (MEAN, SE,
LOWER, UPPER, N).

To call external SAS code for analysis the following syntax should be
used:

analyzeData(analysisCode = "emax.sas", software="SAS", macroCode =
macrocode)

macroCode functions can be written and tested as before. The SAS output
will be passed back into R and macro-evaluation carried out on each
replicate as though the analysis had been carried out in R.

For the above example we ran the following SAS analysis code. This code
analyses the generated data using a basic PROC NLIN call. Notice that we
are setting the required output MEAN, SE, LOWER, UPPER, N to zero here
and returning only the Emax model parameters. In this example we are
only interested in assessing the model fit parameters against the "true"
values used in data generation. We use the SAS ODS system to create a
dataset of the model parameters.

ods output parameterestimates=parms corrb=corr;

proc nlin;

<span class="Apple-converted-space"> </span>model resp=e0 + (emax \*
dose) / (ed50 + dose);<span class="Apple-converted-space">  </span>\*\*
Specify the form of the Emax equation;

<span class="Apple-converted-space"> </span>parameters e0 = 0 ed50 = 25
emax = 120;<span class="Apple-converted-space">  </span>\*\* Specify
starting values for the parameters;

<span class="Apple-converted-space"> </span>bounds ed50 \> 0; <span
class="Apple-converted-space">  </span>\*\* Set up boundary
conditions.<span class="Apple-converted-space">  </span>Here ED50 must
be positive;

run;

ods output close;

proc transpose data=parms out=tparms prefix=mean;

<span class="Apple-converted-space"> </span>var estimate;

<span class="Apple-converted-space"> </span>id parameter;<span
class="Apple-converted-space"> </span>

run;

proc transpose data=parms out=stderr prefix=se;

<span class="Apple-converted-space"> </span>var estimate;

<span class="Apple-converted-space"> </span>id parameter;<span
class="Apple-converted-space"> </span>

run;

proc sql noprint;

<span class="Apple-converted-space"> </span>create table doses as

<span class="Apple-converted-space">   </span>select unique(dose) as
dose

<span class="Apple-converted-space">   </span>from infile;

<span class="Apple-converted-space"> </span>create table parms2 as

<span class="Apple-converted-space">   </span>select \* from tparms t,
stderr s;

<span class="Apple-converted-space"> </span>create table doseparms as

<span class="Apple-converted-space">   </span>select \* from doses d,
tparms t, stderr s;

<span class="Apple-converted-space"> </span>create table obsvars as

<span class="Apple-converted-space">   </span>select mean(resp) as dsm,
var(resp) as dsv,count(resp) as n

<span class="Apple-converted-space">   </span>from infile

<span class="Apple-converted-space">   </span>group by dose;

quit;

data outfile (drop=\_name\_);

<span class="Apple-converted-space"> </span>retain mean se lower upper
0;

<span class="Apple-converted-space"> </span>set doseparms;

<span class="Apple-converted-space"> </span>mean=0;

<span class="Apple-converted-space"> </span>se=0;

<span class="Apple-converted-space"> </span>lower=0;

<span class="Apple-converted-space"> </span>upper=0;

<span class="Apple-converted-space"> </span>n=0;

<span class="Apple-converted-space"> </span>run;

\

**Troubleshooting the analysis code**

To troubleshoot analysis / interim code / macro-evaluation code you can
read in the individual dataset with this code:

data \<- readData( dataNumber = 1, dataType = "Replicate")

then run the analysisCode function over this

**EmaxCode(data)**

Output from the **EmaxCode** function should be the micro-evaluation
dataset. However if this doesn’t return the right dataset it’s easier to
troubleshoot than running all 100 of the replicates…

You can check the **macroCode** function by reading in the
micro-evaluation dataset:

data\<-read.csv("./microevaluation/micro0001.csv")

and then run the **macroCode** function over this:

**macroCode(data)**

Once you’re happy with this, you can go on to look at how the
**interimCode** works by running

analyzeRep(analysisCode=emaxCode,interimCode=interimcode,replicate=1)

This will do the full analysis / micro-evaluation step at interims as
well as on the full dataset. **NOTE** that you need to specify interim
analysis proportions in the generated data BEFORE you analyze the
replicate dataset. If subjects are not assigned to interim cuts in the
generated data, the interim analysis steps will not be carried out (the
**analyzeData** function assumes that all subjects are in the FULL
dataset).
