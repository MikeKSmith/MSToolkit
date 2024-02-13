
test.data.response.createResponseVariable <- function() {
 
 # data is required and must be a data frame 
  checkException(createResponseVariable( equation = "X+Z" )             )
  checkException(createResponseVariable( data = list(X=1, Z=3), "X+Z" ) ) 
 
 # correct behaviour
 checkTrue( all(createResponseVariable( mtcars, "cyl+disp" ) == mtcars$cyl + mtcars$disp ) )
 checkEquals( length(createResponseVariable( mtcars, "cyl+disp" )), nrow(mtcars))
            
 # wrong subset size
 checkException( createResponseVariable( mtcars, "disp[1:50]" )  )  
 checkException( createResponseVariable( mtcars, "rnorm(10)" )  )  
 
 # wrong code
 checkException( createResponseVariable( mtcars, "Sepal.Length+Species" )  )  
 checkException( createResponseVariable( mtcars, "{" )  )     
 
 # Test the "pretest" functionality
 preFalse <- try(createResponseVariable( mtcars, "cyl+", preTest = FALSE ), silent = TRUE)[1]
 preTrue <- try( createResponseVariable( mtcars, "cyl+", preTest = TRUE ), silent = TRUE)[1]
 checkTrue(length(grep("subset", preFalse)) == 0) 
 checkTrue(length(grep("subset", preTrue)) > 0) 
 
}    

test.data.response.addResidualError <- function(){
  resp <- rep(0, 100)                                          
  # response and covariance are required
  checkException( addResidualError( resp ) )
  checkException( addResidualError( covariance = "1" ) )
  checkException( addResidualError(  ) )
  
  # errStruc is additive, proportional or a function
  checkException( addResidualError( resp, "1", errStruc = "q") )
  myf <- function(x) log(x)
  checkException( addResidualError( resp, "1", errStruc = myf) )
  myf <- function(x, y) tail( (x+y) )
  checkException( addResidualError( resp, "1", errStruc = myf) )
 
  ## seed test
  r1 <- addResidualError( resp, "1", errStruc = "A", seed = 1 )
  junk <- rnorm(100) + runif(100)
  r2 <- addResidualError( resp, "1", errStruc = "A", seed = 1 )
  checkTrue( all(r1 == r2 ) ) 
  
}

test.data.response.createResponse <- function() {
  
  checkEquals( createResponse( mtcars, "mpg+cyl")[,1], mtcars$mpg + mtcars$cyl   )
  checkTrue( all( createResponse( mtcars, "mpg+cyl")$RESPOMIT == 0 ) )
  
  # check the distribution
  checkException( createResponse( mtcars, "mpg+cyl", distribution = "gre")  )
  
  # wrong names
  checkException( createResponse( mtcars, "mpg+cyl", name = "'fea")  )
  checkException( createResponse( mtcars, "mpg+cyl", flagName = "-9ea")  )
  checkException( createResponse( mtcars, "mpg+cyl", flagName = "XX", name = "XX")  )

  # digits should be one positive number
  checkException( createResponse( mtcars, "mpg+cyl", digits = "-1")  )
  checkException( createResponse( mtcars, "mpg+cyl", digits = "1,2")  )
  
  # range code
  checkException( createResponse( mtcars, "mpg+cyl", range = "mpg")  )
  
  # range code
  resp <- createResponse( mtcars, "mpg", range = "mpg < 4" )
  checkTrue( all( resp$PAROMIT[ mtcars$mpg < 4 ] == 1 ) )
  
  resp <- createResponse( mtcars, "mpg", range = "RESP < 4" )
  checkTrue( all( resp$PAROMIT[ mtcars$mpg < 4 ] == 1 ) )
  
  checkEquals( dim(resp), c( nrow(mtcars), 2) )
  
}

# SF issue 10
# Tue Jul 24 11:06:43 BST 2007 @462 /Internet Time/
test.data.response.sf10 <- function( ){
  
  myData <- expand.grid( X = 1:2, Y = 1:2, Z = 1:2 )[ rep( 1:6, 10000), ]
   
  checkException( createResponse(data = myData, equation   = "X+Y+Z", 
    invLink = function(x)  rep(1.3, length(x)) , distribution = "b" ), 
    msg = "invLink should generate probabilities" )

  out <- createResponse(data = myData, equation   = "X+Y+Z", 
    invLink = function(x)  rep(.9, length(x)) , distribution = "b" )$RESP  
  checkTrue( binom.test( table(out), p= .1 , alternative = "two.sided")$p.value > .05, 
    msg = "stats test for the binomial response where invLink is given")  
  
  out <- createResponse(data = myData, equation   = "X+Y+Z", 
    invLink = function(x)  rep(1, length(x)) , distribution = "b" )$RESP  
  checkTrue( all(out == 1), 
    msg = "stats test for the binomial response where invLink is given (1)")
  out <- createResponse(data = myData, equation   = "X+Y+Z", 
    invLink = function(x)  rep(0, length(x)) , distribution = "b" )$RESP  
  checkTrue( all(out == 0), 
    msg = "stats test for the binomial response where invLink is given (0)")
    
}

# bug fix cause by not usnug drop = FALSE
# Wed Jul 25 11:27:38 BST 2007 @477 /Internet Time/
test.data.response.dropbug <- function(){
  myData <- data.frame( X = rnorm(10) )
  out <- try( createResponse(myData, "X" ) )
  
  checkTrue( class(out) != "try-error", 
    msg = "drop = FALSE bugfix" )
  
  checkEquals( out$RESP, round(myData$X,3), 
    msg = "drop = FALSE bugfix sanity check")  
}


