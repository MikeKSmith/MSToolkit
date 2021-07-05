
test.data.interim.1 <- function() {
  ## unit tests for the interim component
  
  ## check the handling of method
  checkException( createInterims( 10, method = "a")  )
  
  ## handle the subjects argument
  checkException( createInterims( -10) )
    
  ## proportion must not go beyond 1
  checkException( createInterims(5, proportion  = ".1,1.2" ) )
  
  ## proportion not in ascending order
  checkException( createInterims(5, proportion  = ".4,.3" ) )
  
  
}

test.data.interim.2 <- function() {
  a <- createInterims(10, proportion  = ".1,.3,.6" )
  ## size of output
  checkEquals( nrow(a), 10 )
  checkEquals( names(a), c("SUBJ", "INTERIM") )
  checkEquals( a[,1], 1:10 )
  
  # check rejection of wrong names for idCol or interimCol
  checkException( createInterims(10, proportion  = ".1,.3,.6", idCol = ".1" ) )
  checkException( createInterims(10, proportion  = ".1,.3,.6", interimCol = ".1" ) )
  checkException( createInterims(10, proportion  = ".1,.3,.6", interimCol = "XX", idCol = "XX" ) )
  
  checkEquals( names(createInterims(10, proportion  = ".1,.3,.6", idCol = "IDCOL" ))[1] , "IDCOL" ) 
}

test.data.interim.3 <- function() {

  ## check the sample method
  b <- createInterims(1000, proportion  = ".1", method = "Sample" )
  # check unique values
  checkEquals( sort(unique(b[,2])), 1:2 )
  
  # statistical proportion test
  checkTrue( binom.test( table(b[,2]), n = 1, p = .1  )$p.value > .05  )
}


test.data.interim.5 <- function() {

  ## check handling of missing proportion
  a <- createInterims(10)
  checkTrue( all(a[,2]==1) )
  
}

