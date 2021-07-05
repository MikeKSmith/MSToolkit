

### basic tests on the arguments
test.data.treatments.1 <- function( ){
  
  ## need `sequence` or `doses`
  checkException( createTreatments() ) 
     
  ## invalid names
  checkException(createTreatments(doses = c(0,30), doseCol = "12invalid") )
  checkException(createTreatments(doses = c(0,30), trtCol  = "+4353242invalid") )
  checkException(createTreatments(doses = c(0,30), timeCol = "~'lefw12invalid") )
  
  # check if doses and times can be converted to numeric
  checkException( createTreatments(doses = c(0,30), times = c("a", "b")) )
  checkException( createTreatments(doses = c("da", "fes") ) )
}

### tests when using parallel design
test.data.treatments.parallel <- function() {
  
  ## check correct creation when all positive times
  tr <- createTreatments(doses = c(0,15,30), times = c(1,2,3) )
  checkTrue( is.data.frame(tr) )
  checkTrue( all( dim(tr) == c(9,3)) )
  checkTrue( all( names(tr) == c("TRT", "TIME", "DOSE")))
  checkTrue( all( sort(unique(tr$DOSE)) == c(0,15,30)) )
  checkTrue( all( tr$DOSE[tr$TRT == 1] == 0 ) )
  checkTrue( all( tr$DOSE[tr$TRT == 2] == 15 ) )
  checkTrue( all( tr$DOSE[tr$TRT == 3] == 30 ) )
  
  ## check correct when times 0 or negative
  tr <- createTreatments(doses = c(0,15,30), times = c(-1,0,1,2,3) )
  
  ## run in test
  checkTrue(  all(tr$DOSE[tr$TIME < 0] == 0 ) )
  
  
}
### tests when using crossover design
test.data.treatments.crossover <- function() {

  seqMat <- rbind(
     c(   0,    0,    0) ,
     c(   0,   20,   10) ,
     c(  10,   10,    0) ,
     c(  20,    0,   20) )
  ## not enought times
  checkException( createTreatments( sequence = seqMat, times = c(1,2)) )
  
  ## wrong matrix for run in
  checkException( createTreatments( sequence = seqMat, times = c(-2,-1,1,2)) )
  
  tr <- createTreatments( sequence = seqMat, times = c(0,1,2,3)) 
  ## run in test
  checkTrue( all(tr$DOSE[tr$TIME <= 0] == 0 ) ) 
  for(i in 1:3) checkTrue( all(tr$DOSE[tr$TRT==i] == seqMat[,i])  )
  
  seqMat <- rbind(
     c(   "a",   "b",   "s") ,
     c(   "a", "a", "a") )
  checkException( createTreatments( sequence = seqMat) )
  
  # More unit tests for createTreatments
  x <- createTreatments(sequence=cbind(c(0, 15, 30), c(15, 30, 0) ,c(30, 15, 0)))
  checkTrue(identical(x, data.frame(TRT=rep(1:3, each=3), TIME=rep(1:3, 3), DOSE=c(0, 15, 30, 15, 30, 0, 30, 15, 0))))

  x <- createTreatments(type="c", times=0:3, sequence=cbind(c(0, 15, 30), c(15, 30, 0) ,c(30, 15, 0)))
  checkTrue(identical(x, data.frame(TRT=rep(1:3, each=4), TIME=rep(0:3, 3), DOSE=c(0, 0, 15, 30, 0, 15, 30, 0, 0, 30, 15, 0))))

  x <- createTreatments(doses=1:2, times=0:1, trtCol="X", timeCol="Y", doseCol="Z")
  checkTrue(identical(x, data.frame(X=rep(1:2, each=2), Y=rep(0:1, 2), Z=c(1, 1, 2, 2))))
  
}

