#setwd("C:/Documents and Settings/smith_mk/my documents/working documents/MSToolkit")
library(MSToolkit)
pdf("Testscript2.pdf")
## identifies and creates TRT vs DOSE dataset
## TRT is treatment identifier (1:#doses)
## DOSE is (numeric) dose level.
createTreatments(c(0,5,25,50,100))

## Can create crossover designs by specifying sequence of treatments
## TRT = sequence in this case (used for subject allocation to sequences
## TIME = treatment period
## DOSE = (numeric) dose level.
createTreatments(sequence=cbind(c(0, 15, 30), c(15, 30, 0), c(30, 0, 15)))

## Can create run-in periods
## if TIME <= 0 then DOSE=0
createTreatments(doses=c(0, 15, 30), times=0:2)
createTreatments(doses=c(0, 15, 30), times=-2:2)

## Allocates subjects randomly to treatments or sequences
## NOTE - because random allocation, may not allocate to ALL doses.
## ID = subject ID
## TRT = tretment identified (as specified above in createTreatments)
set.seed(987654321)
allocateTreatments(trts = 3, subjects = 6, ordered = TRUE)

## Testing allocation ratio
foo1<-allocateTreatments(trts = 3, subjects = 100000, prop=c(0.5,0.25,0.25),order=T)
tapply(foo1$SUBJ,foo1$TRT,length)/100000

## Testing seed
allocateTreatments(trts = 3, subjects = 10, ordered = TRUE,seed=123)
allocateTreatments(trts = 3, subjects = 10, ordered = TRUE,seed=123)

## Allocating fixed numbers to each treatment
## Doesn't need order=T, but this helps to check.
allocateTreatments(trts = 3, subjects = c(2, 2, 3),order=T)
allocateTreatments(trts = 3, subjects = c(2, 2, 3))

## Creates continuous covariates (usually called from createCovariates)
## Testing versus TS2 population covarates of AGE and BWT 
createContinuousCovariates( 30, mean = "40,60", covariance=cbind(c(20,0),c(0,15)), 
        names = c("AGE", "BWT"), digits=0)

## Checking distributions
foo2<-createContinuousCovariates( 10000, mean = "40,60", covariance=cbind(c(20,0),c(0,15)), 
        names = c("AGE", "BWT"))
apply(foo2,2,mean)
apply(foo2,2,var)

## Checking using ranges
foo3<-createContinuousCovariates( 10000, mean = "40,60", covariance=cbind(c(150,0),c(0,60)), 
        names = c("AGE", "BWT"),seed=123)
apply(foo3,2,summary)

## Applying ranges
foo4<-createContinuousCovariates( 10000, mean = "40,60", covariance=cbind(c(150,0),c(0,60)), 
        names = c("AGE", "BWT"),range=c("18<AGE<65","45<BWT"),seed=123)
apply(foo4,2,summary)
foo3[18,]
foo4[18,]

## Applying extreme ranges
foo5<-createContinuousCovariates( 10000, mean = "40,60", covariance=cbind(c(150,0),c(0,60)), 
        names = c("AGE", "BWT"),range=c("40<AGE"))
apply(foo5,2,summary)

## Checking maxDraws behaviour
## Trap error using try so that the remainder of the script completes.
foo6<-try(createContinuousCovariates( 10000, mean = "40,60", covariance=cbind(c(20,0),c(0,15)), 
        names = c("AGE", "BWT"),range=c("AGE<0")),silent=T)
foo6[1]

## Checking discrete covariates
## Discrete value distributions from ZDV with Genetics example in TS2
CCR5xCCR2 <- data.frame(CCR5=rep(c("plus_plus","plus_D32","D32/D32"),each=3),
                        CCR2=rep(c("pl_pl","pl_64l","64l/64l"),3),
                        PROB=c(0.7744,0.0352,0.0004,0.0352,0.1416,0.0032,0.0004,0.0032,0.0064))
print(CCR5xCCR2)
createDiscreteCovariates( 100 , probArray = CCR5xCCR2 )

## Sampling from external data file
## sameRow=T samples subject level vectors of covariates
## Sampling with replacement
#foo7 <- createExternalCovariates( 20, names = "AGE,SEX,WT,HT,RACE", 
#    file = "pkv14.dta", refCol = "ID" )
#print( foo7 )

#checkfoo7<-read.table("pkv14.dta",header=T,sep=",")
#checkfoo7<-checkfoo7[match(unique(checkfoo7$ID),checkfoo7$ID),]
#checkfoo7<-checkfoo7[checkfoo7$ID%in%foo7$ID.refCol,]
#sort(foo7$AGE)
#sort(checkfoo7$AGE)

## Checking parameter distribution creation
library(DoseResponse)
data(edd)
nls.fit<-nls(ch8 ~ e0 + (emax * dose)/(dose + ed50), data=edd,
                        start = c(ed50=25,e0=5,emax=20), control = nls.control(
                        maxiter = 100),trace=TRUE,na.action=na.omit)

createNormalParameters(5, "ED50,E0,EMAX", mean = coef(nls.fit), covariance = vcov(nls.fit))
## 5 subjects created with the same E0,ED50 and Emax value but these values randomly sampled 
## from the appropriate distribution

## Checking what happens if no covariance matrix specified
createNormalParameters(5,"ED50,E0,EMAX",mean=c(50,0,100))
## Needs covariance matrix.  Should probably default to covariance = 0 if not specified.
createNormalParameters(5,"ED50,E0,EMAX",mean=c(50,0,100),covariance=matrix(0,nrow=3,ncol=3))

## Checking sampling from external file  
#foo8<- createExternalParameters( subjects = 20, names ="VMAX,KM,MTT,BIO", 
#    file = "patab25")
## Assumes external file is CSV format.  PATAB25 above isn't.  
## This will be a problem with NONMEM Table files.  Request raised.


## Checking creation of response variable
Tx1<-createTreatments(c(0,5,25,50,100))
Tx2<-allocateTreatments(trts=5,subjects=100,ordered=T)
Tx3<-merge(Tx1,Tx2)
Parms1<-createNormalParameters(100,"ED50,E0,EMAX",mean=c(50,0,100),covariance=matrix(0,nrow=3,ncol=3))
Data1<-merge(Tx3,Parms1)
attributes(Data1)
Data2<-createResponse(Data1,"E0+(EMAX*DOSE)/(ED50+DOSE)",covariance=100)
Data3<-cbind(Data1,Data2)
plotD(Data3$RESP,Data3$DOSE,se=T)
plot(Data3$DOSE,Data3$RESP)
emaxalt(Data3$RESP,Data3$DOSE)
nls.fit<-nls(RESP~E0+(EMAX*DOSE)/(ED50+DOSE),data=Data3,start=list(E0=0,ED50=50,EMAX=100))
summary(nls.fit)
##  Responses have been created appropriately, NOTE that covariance in createResponse requires variance
##  Normally we would assume std.dev. for rnorm(...) etc.

## Checking other link functions
## Creating dataset based on Kyphosis example from S-Plus online examples
Tx1<-createTreatments(c(2,3,4,5,6))
Tx2<-allocateTreatments(trts=5,subjects=100,ordered=T)
Tx3<-merge(Tx1,Tx2)

## Checking Poisson regression using Salmonella example dataset
Tx1<-createTreatments(c(0,10,33,100,333,1000))
Tx2<-allocateTreatments(trts=6,subjects=120,order=T)
Tx3<-merge(Tx1,Tx2)
Parms3<-createNormalParameters(120,"ALPHA,BETA,GAMMA",mean=c(2.116,0.3271,-0.001038),covariance=matrix(0,nrow=3,ncol=3))
Data8<-merge(Tx3,Parms3)
Resp3<-createResponse(Data8,"ALPHA+BETA*log(DOSE+10)+GAMMA*DOSE",invLink=exp,dist="p",cov=0,seed=123)
Data9<-cbind(Data8,Resp3)
tapply(Data9$RESP,list(Data9$DOSE),mean)
plotD(Data9$RESP,Data9$DOSE)

Resp4<-createResponseVariable(Data8,"ALPHA+BETA*log(DOSE+10)+GAMMA*DOSE")
Resp5<-exp(Resp4)
Resp6<-sapply(Resp5,rpois,n=1)
tapply(Resp6,list(Data9$DOSE),mean)
plotD(Resp6,Data9$DOSE)
## Looks fine.

## Checking MCAR
Data10<-createMCAR(Data9,prop=0.1)
mean(Data10$MISSING)

## Checking dropout
## Neither of these functions are currently accepted by createDropout
## checkDropOutFunc function expects prop?
myDrop<-function(x){sapply(x,rbinom,n=1,size=1,prob=exp(-1+0.5*x$DOSE)/(1+exp(-1+0.5*x$DOSE)))}
myDrop2<-function(data){data$DOSE>100}
createDropout(Data9,dropFunc=myDrop2)

## Checking interim assignment
Data11<-cbind(Data9,createInterims(subjects=120,proportion=c(0.25,0.5,0.75)))
table(Data11$DOSE,Data11$INTERIM)

generateData( replicateN = 5, subjects = 120, treatDoses = c(0, 33, 100, 333, 1000), 
  genParNames = "ALPHA,BETA,GAMMA", genParMean =c(2.116,0.3271,-0.001038), genParVCov = diag( c(0.138,0.0104,0.000000211) ), 
  respEqn = "ALPHA+BETA*log(DOSE+10)+GAMMA*DOSE",  respVCov = 0.0683, respInvLink=exp, respDist="p", seed=123)

Parms4<-createNormalParameters(120,"ALPHA,BETA,GAMMA",mean=c(2.116,0.3271,-0.001038),covariance=diag( c(0.138,0.0104,0.000000211) ),seed=123)
Data8<-merge(Tx3,Parms4)
Resp7<-createResponse(Data8,"ALPHA+BETA*log(DOSE+10)+GAMMA*DOSE",invLink=exp,dist="p",cov=0.0683,seed=123)
Resp7b<-createResponse(Data8,"ALPHA+BETA*log(DOSE+10)+GAMMA*DOSE",dist="p",cov=0.0683,seed=123)
Data10<-cbind(Data8,Resp7)
tapply(Data10$RESP,list(Data10$DOSE),mean)
plotD(Data10$RESP,Data10$DOSE)
dev.off()

