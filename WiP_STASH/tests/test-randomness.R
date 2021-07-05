context("Check than random number seeding is behaving appropriately (slow test)")

test_that("We get decent randomness if we don't start generateData with a seed", {
  # generateData was calling .deriveFromMasterSeed which just samples from 1:999
  # All other internal seeds are derived (deterministically) from the value sampled
  # So only 999 possible outcomes for a given simulation
  
  # We test this by generating a series of replicates of replicates
  # each starting with default seed, and calculate the hash of each replicate0001.csv
  # that results.  The then check there are no repetitions in this.
  
  set.seed(3)
  
  getRepMD5 <- function(fname){
    # Return the MD5 of a replicate data
    require(digest)
    return(digest(fname, algo="md5", serialize=FALSE, file=TRUE))
  }
  
    
  testgenmd5 <- function(x){
    
    emax.fn<-function(data){
      with(data,{
        e0.time <- e0 + 0.1*TIME
        e0.time+(emax*DOSE)/(ed50+DOSE)
      }
      )
    }
    
    
    e0<-0.3
    emax<-0.55-e0
    
    ed50<-3
    # 320, 80, 4
  generateData(replicateN=1,subjects=160,treatSubj=rep(40,4),treatDoses=c(0,3,10,30),
               treatPeriod=seq(1,2),
               genParNames="ed50,e0,emax", genParMean=c(ed50,e0,emax), genParVCov=10,
               respEqn=emax.fn, respVCov = 100)   
  md5 <- getRepMD5("./ReplicateData/replicate0001.csv")
  removeDirectories("ReplicateData") 
  return(md5)
  }

  md5s <- sapply(1:100, testgenmd5)
  
  expect_true(max(rle(sort(md5s))$lengths) == 1)
  
})