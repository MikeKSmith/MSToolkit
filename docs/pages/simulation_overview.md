---
layout: page
title: Simulation Overview
description: An introduction to trial simulation, model parameter uncertainty
and evaluating operating characteristics of trial + analysis method.
---

Clinical trial simulation (CTS) involves the generation of trial
outcomes for artificial subjects based on given inputs, data generation
models which use these inputs to generate outcome data and the
subsequent analysis of the generated dataset. The data generation model
can include models describing the time-course of disease or clinical
endpoint over the period of study in the trial and how this time-course
is affected by drug treatment. These models may be mechanistic,
reflecting biological systems 14 but they may also be empirical,
describing trends seen in the pre-existing data either from previous
clinical trials or based on meta-analysis of related literature data.

CTS also requires specification of a trial design including doses given,
dosage regimens, number of subjects to be studied, how these subjects
are allocated to the different treatment arms and patient population
characteristics if these influence the outcome. Factors that affect the trial
execution such as the presence of interim analyses, and subject level
processes describing protocol adherence and missing data are also
frequently used. All of these elements can be incorporated into a
clinical trial simulation using appropriate mathematical models based on
available data. Often the mathematical models for the different elements
of the data generation process are developed separately. 
The models describing these processes can often be validated only through 
simulation of existing trial designs (often using existing trial data structures
) and comparison of the resulting simulated data with the observed data. This
model validation process is essential before embarking on simulations
for novel trial designs and new scenarios.

The “classical” approach to clinical trial simulation is to consider the
treatment effect fixed and known - it could also conceivably correspond
to a null hypothesis - and then simulations are performed to create
random variate response observations for individual subjects
corresponding to the specific trial design scenario and data generation
inputs described above. Thus the only difference between trial
replicates is the random variability in response due to new subjects
being drawn from the random variate using the fixed treatment difference
in its mean. An alternative approach, and one that is increasingly used
in pharmacometrics for predictive simulation, involves drawing the
treatment effect (or data generation model parameters) from a
distribution of plausible values frequently derived through analysis of
prior data. Each replicate of the simulation then has a different
treatment effect or set of model parameters, and thus has a different
“true” drug effect. In this latter case we may want to know what
decision would be made for the “truth” i.e. for this particular drug
effect or set of model parameters and compare this with the decision
made for the particular trial design and analytical method
  
In CTS, the performance of different analytical methodologies can be
evaluated on the same dataset, thus enabling comparison of performance
metrics for these analytical methods using identical datasets and
comparison against the deterministic outcome for given inputs using the
data generation model (truth). The probability of making correct or
incorrect decisions (operating characteristics) can then be
characterised either using classical notions of statistical significance
and Type I and Type II errors or with reference to clinical decision
criteria around sufficient efficacy or acceptable tolerability .

CTS can be used to assess how often the correct dose
choice would be made for a given design, analytical method and
dose-selection decision criteria, and calibrate this against the “truth”
for the current inputs and data generation model. For a given set of
inputs and data generation model we will know what the “correct”
decision would have been using the same dose-selection criteria. Through
CTS, the sensitivity of the conclusions to changes in the data
generation models, trial design or the assumptions made in the data
analytic and decision making processes can be examined. CTS aims to
answer “What if…” questions such as “What if data are generated from a
nonlinear model but then analysed assuming a linear model?” In this
case, CTS can be used to examine the robustness of scenarios where the
assumptions of the data generation and analytic models are different,
comparing these against a “base case” where assumptions are maintained
in both the data generation and data analysis methodology.

When generating data in CTS it is important to
understand and to be able to quantify how different sources of
variability (both random variability and covariate effects) impact the
endpoint of interest. When we use parametric models to generate a
response for an individual subject we may wish to include covariate
relationships to describe how subjects differ e.g. response as a
function of age, but if generating repeated measures within an
individual subjects, it is important to consider which factors impact
the response variables - within subject and between subject effects. It
is often helpful to think of the form of the analytical model used to
analyse such data and then reconstruct the sources of variability
between and within subjects from this model.

When examining the operating characteristics of trials,
it is also frequently necessary to examine the robustness of designs and
analytical methods to cases when the "base" assumptions do not hold. For
example if we make an assumption of linear response in the analysis,
what are the operating characteristics of a given trial design when we
generate data with nonlinear response?

MSToolkit also allows us to examine the operating
characteristics of trials when we vary the parameters of the data
generation process between trial replicates. If we keep the parameter
values constant across trial replicates then we assume that the only
difference between trials is in the random between and within subject
effects i.e. only the patient population changes. Under this assumption
the treatment effects are fixed - which would equate to a "frequentist"
notion of fixed delta for sample sizing. If we vary the data generation
parameters between trial replicates we can extend our trial simulation
to examine a more "Bayesian" notion that the "true" treatment effect may
not be known with certainty but that the data generation process in a
future trial is driven by the current state of knowledge about the
parameters of the data generation model. Thus, when we generate
parameters for generating data, we allow users to specify not just the
mean of these parameters but a covariance matrix, specifying our between
trial replicate uncertainty in these parameters.

In the hierarchy of data generation then we have three
levels where sources of uncertainty and variability can enter: between
trial replicate variability, between subject variability, and within
subject variability.

[Williams and Ette 2003 – 'Determination of model appropriateness' in "Simulation for Designing Clinical Trials" pp74-103, Kimko and Duffull eds, Marcel Dekker, NY](https://www.crcpress.com/Simulation-for-Designing-Clinical-Trials-A-Pharmacokinetic-Pharmacodynamic/Kimko-Duffull/p/book/9780824708627)
[Decision-Making in Drug Development: Application of a Model Based Framework for Assessing Trial Performance. Smith MK, French JL, Kowalski KG, Hutmacher MM, Ewy W. in "Clinical Trial Simulation - applications and trends" pp61-83, Kimko and Peck eds, Springer, NY](http://www.springer.com/gp/book/9781441974143)
[A Quantitative Approach for Making Go/No-Go Decisions in Drug Development. Chuang-Stein C, Kirby S, French JL, Kowalski KG, Marshall SF, Smith MK, Bycott P, Beltangady M.Therapeutic Innovation & Regulatory Science  Vol 45, Issue 2, pp. 187 - 202](http://journals.sagepub.com/doi/abs/10.1177/009286151104500213)
[Modeling and simulation to support dose selection and clinical development of SC-75416, a selective COX-2 inhibitor for the treatment of acute and chronic pain. Kowalski KG, Olson S, Remmers AE, Hutmacher MM. Clin Pharmacol Ther. 2008 Jun;83(6):857-66.](https://www.ncbi.nlm.nih.gov/pubmed/17882158);
[Model-based drug development. Lalonde R, Kowalski K, Hutmacher M, Ewy W, Nichols D, Milligan P, et al. Clin Pharmacol Ther. 2007 Jul;82(1):21-32](https://www.ncbi.nlm.nih.gov/pubmed/17522597);
[Model-based drug development: a rational approach to efficiently accelerate drug development. Milligan PA, Brown MJ, Marchant B, Martin SW, van der Graaf PH, Benson N, Nucci G, Nichols DJ, Boyd RA, Mandema JW, Krishnaswami S, Zwillich S, Gruben D, Anziano RJ, Stock TC, Lalonde RL. Clin Pharmacol Ther. 2013 Jun;93(6):502-14. doi: 10.1038/clpt.2013.54.](https://www.ncbi.nlm.nih.gov/pubmed/23588322);