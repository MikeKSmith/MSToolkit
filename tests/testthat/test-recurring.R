context("testthat covariate tests")

test_that("Recurring fractions allowed in arm assignment probs", {
  
  emax.fn<-function(data){
    with(data,{
      e0.time <- e0 + 0.1*TIME
      e0.time+(emax*DOSE)/(ed50+DOSE) 
    }
    )
  }
  
  
  expect_null(generateData(replicateN=2,subjects=320,treatSubj=rep(80,4),treatDoses=c(0,3,10,30),
                           treatPeriod=seq(1,14),
                           genParNames="ed50,e0,emax", genParMean=c(250,10,20), #genParVCov=vcov,
                           respEqn=emax.fn,
                           disCovNames = "AGENT",
                           disCovVals = "A,B,C",
                           disCovProb = paste(rep(as.character(1/3), 3), collapse=","),
                           seed=123456) 
  )
  
  # Check probabilities that really don't sum to 1 fail
  expect_error(generateData(replicateN=2,subjects=320,treatSubj=rep(80,4),treatDoses=c(0,3,10,30),
                           treatPeriod=seq(1,14),
                           genParNames="ed50,e0,emax", genParMean=c(250,10,20), #genParVCov=vcov,
                           respEqn=emax.fn,
                           disCovNames = "AGENT",
                           disCovVals = "A,B,C",
                           disCovProb = "0.25,0.25,0.25", # Probs dont sum to 1
                           seed=123456) 
  )
  
})