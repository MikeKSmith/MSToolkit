
test.data.missing.mcar <- function() {

  tdata <- data.frame( 
    SUBJ = rep(1:3, each = 3), 
    TIME = rep(0:2, 3) )
  
  checkTrue( all( createMCAR( tdata, prop = 0, flagName = "ABC")$ABC == 0 ))
  checkTrue( all( createMCAR( tdata, prop = 1)$MISSING == 1 ))
  
  checkException( createMCAR( tdata, prop = 100) )
  checkException( createMCAR( tdata, prop = -10) )
  
  out <- createMCAR( tdata, prop = 1, rule = "TIME > 0" )
  checkTrue( all( out$MISSING[ tdata$TIME == 0 ] == 0 ))
  checkTrue( all( out$MISSING[ tdata$TIME >  0 ] == 1 ))
  
  checkException( createMCAR( tdata, prop = 1, rule = "NOTEXISTS > 0" ) ) 
  
  checkException( createMCAR( tdata, prop = 1, rule = "TIME > 10" ) ) 
  
  checkException( createMCAR( tdata, prop = 1, flagName = "0e321" ) ) 
  
  tdata <- expand.grid( SUBJ = 1:100, TIME = 0:4 )
  tdata$MISSING <- sample( c(0,1) , size = 500, replace = TRUE)
  out <- createMCAR( tdata, prop = .5 )
  checkTrue( all( out$MISSING[ tdata$MISSING == 1] == 1 ) )
}       
                     

test.data.missing.drop <- function() {
  
 # test with a correct dropout function 
 dFun <- function(data, prop) sample(0:1, nrow(data), TRUE, c(1-prop, prop))
 testData <- data.frame(
   SUBJ=rep(1:10, each=5), 
   TIME=rep(0:4, 10), 
   VALUE=rnorm(50))

 doData <- createDropout(testData, dFun, prop=0.4, seed=10) 
 
 out <- with( doData, tapply( MISSING, SUBJ, function(x) {  
   w1 <- which( x == 1 ) 
   length(w1) == 0 || all( w1 == seq(to = 5, length.out = length(w1) ) )
 }) )
 checkTrue( all(out) , 
   msg = "check that the dropout is retained until the end")
 
 # tests about the drop out function  
 checkException( createDropout( doData, max ), 
   msg = "wrong function")  
   
 checkException( createDropout( doData, function(data, prop)  1:nrow(data)    ), 
   msg = "wrong function, does not generate 0,1,T,F")  
 
 checkException( createDropout( doData, function(data, prop)  stop("error")    ), 
   msg = "error occuring")  
 
 checkException( createDropout( doData, "thisfunctiondoesnotexist"    ), 
   msg = "function not existing")  
   
   
  # test about invalid names 
 checkException( createDropout( doData, dFun, idCol = "SUB" ), 
   msg = "idcol not in the dataset")
   
 checkException( createDropout( doData, dFun, idCol = "634,4234-=ge5" ), 
   msg = "idcol is invalid")

 checkException( createDropout( doData, dFun, idCol = "ID,SUB" ), 
   msg = "idcol must be of length one")

 checkException( createDropout( doData, dFun, timeCol = "TT" ), 
   msg = "timecol not in the dataset")
   
 checkException( createDropout( doData, dFun, timeCol = "634,4234-=ge5" ), 
   msg = "timecol is invalid")

 checkException( createDropout( doData, dFun, timeCol = "TIME,time" ), 
   msg = "timecol must be of length one")
   
  checkException( createDropout( doData, dFun, flagName = "634,42gr34-=ge5" ), 
   msg = "flagName is invalid")

 checkException( createDropout( doData, dFun, flagName = "MISSING,MISS" ), 
   msg = "flagName must be of length one")
  
   
}

test.data.checkDropuOutFun <- function(){

 dFun <- function(data, prop) sample(0:1, nrow(data), TRUE, c(1-prop, prop))
 testData <- data.frame(
   SUBJ=rep(1:10, each=5), 
   TIME=rep(0:4, 10), 
   VALUE=rnorm(50))
 doData <- createDropout(testData, dFun, prop=0.4, seed=10) 
 
 # tests about the drop out function  
 checkException( checkDropOutFun( max ), msg = "wrong function")     
 checkException( checkDropOutFun( "ghost" ), msg = "does not exist")  
 checkException( checkDropOutFun( ghost ), msg = "does not exist")  
 checkException( checkDropOutFun( function(data, prop)  1:nrow(data), testData ), 
   msg = "wrong function, does not generate 0,1,T,F")  
 checkException( checkDropOutFun( function(data, prop)  stop("error"), doData), msg = "error occuring")  
 checkTrue( checkDropOutFun( function(data, prop)  rep(1, nrow(data)), testData ))
 checkException( checkDropOutFun( function(dta, prop)  rep(1, nrow(data)), testData ))
 dFun <- function(data, ...) rep(1, 5)
 checkTrue( checkDropOutFun( dFun, testData ))
 checkException( checkDropOutFun( dFun, testData, useSubset = FALSE ))
 checkException( checkDropOutFun( dFun, testData, sizeSubset = 6))
 
}




