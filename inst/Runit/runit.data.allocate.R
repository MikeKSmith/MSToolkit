test.data.allocate.inputs <- function(){
  ## invalid names for idCol or trtCol
  checkException( allocateTreatments(  trts = 4, idCol = "}"  )  )
  checkException( allocateTreatments(  trts = 4, trtCol = "}" )  )
  
  # negative subjectsT
  checkException( allocateTreatments( trts = 4, subjects = -1) ) 
  
  # wrong proportions
  checkException( allocateTreatments( trts = 4, subjects = 10, prop = ".3,.7") ) 
  checkException( allocateTreatments( trts = 4, subjects = 10, prop = ".3,.3,.1,2") ) 
  
  # wrong subjects
  checkException( allocateTreatments( trts = 4, subjects = c(10,10), prop = ".3,.3,.1,.3") ) 
  
} 

test.data.allocate.ordered <- function(){
  al <- allocateTreatments( trts = 4, subjects = 10, prop = ".3,.3,.1,.3", ordered = TRUE)
  checkTrue( !is.unsorted(al$TRT) )
  
  al <- allocateTreatments( trts = 4, subjects = c(2,2,3,4), ordered = TRUE)
  checkTrue( !is.unsorted(al$TRT) )
  checkTrue( all( table(al[,2]) == c(2,2,3,4) ))
  
  al <- allocateTreatments( trts = 4, subjects = c(2,2,3,4), ordered = FALSE)
  checkTrue( all( table(al[,2]) == c(2,2,3,4) ))
  checkTrue( all(names(al) == c("SUBJ", "TRT") ) )
  
  al <- allocateTreatments( trts = 4, subjects = c(2,2,3,4), trtCol = "tr", idCol = "ID")
  checkTrue( all(names(al) == c("ID", "tr") ) )
 
}

test.data.allocate.names <- function(){
 checkException( allocateTreatments( trts = 6, subjects = 100, trtCol = "XX", idCol = "XX") )  
}


test.data.allocate.vectorOfVals <- function(){
	al <- allocateTreatments( trts = 1:2, subjects = 10 )
	checkTrue( all(1:2 %in% al$TRT))
}

test.data.allocate.repeatedTreatments <- function() {
	
	generateData(5, 20, treatSubj = c(3, 4, 3), treatDoses = c(0, 15, 30), respEqn = "DOSE")
	x <- readAllData()
	checkTrue( all(sapply(split(x$TRT, x$Replicate), function(x) all(x[1:10] == x[11:20]))) )
	checkTrue( !all( x$TRT [ x$Replicate == 1 ] == x$TRT [ x$Replicate == 2 ]) )

	generateData(5, 20, treatSubj = c(3, 4, 3), treatDoses = c(0, 15, 30), respEqn = "DOSE", treatDiff = FALSE)
	x <- readAllData()
	checkTrue( all(sapply(split(x$TRT, x$Replicate), function(x) all(x[1:10] == x[11:20]))) )
	checkTrue( all( x$TRT [ x$Replicate == 1 ] == x$TRT [ x$Replicate == 2 ]) )
	
}
