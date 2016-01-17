
test.interimAnalysis <- function(){
	
	checkException( interimAnalysis( diag(3), "x" ) , 
			"data is not a a data frame ")
	
	checkException( interimAnalysis( mtcars, "Sepal.Length" ), 
			msg = "code generating error" )
	
	checkException( interimAnalysis( mtcars, "mpg + 2" ), 
			msg = "code must be a function" )
	
	checkException( interimAnalysis( mtcars, "thatFunctionDoesNotExist" ), 
			msg = "code must be a function that exists" )
	
	checkException( interimAnalysis( mtcars, function(data) list( wrongname = data$mpg + 2) ), 
			msg = "wrong list name" )
	
	checkEquals( interimAnalysis( mtcars,  function(data) list() ) , list() , 
			msg = "empty list")
	
	checkException( interimAnalysis( mtcars, function(data) list( DROP = 1, STOP = FALSE, DOSE = 3) ), 
			msg = "too big list" )
	
	checkException( interimAnalysis( mtcars, function(data) list( DROPME = 1, STOPME = FALSE) ), 
			msg = "Incorrect list element names" )
	
	checkException( interimAnalysis( mtcars, function(data) list( DROP = 'a', STOP = FALSE ) ), 
			msg = "DROP must be number or a logical" )
	
	checkException( interimAnalysis( mtcars, function(data) list( DROP = c(0,10), STOP = c(FALSE, TRUE) ) ), 
			msg = "STOP must be a logical of length 1" )
	
	checkException( interimAnalysis( mtcars, function(data) list( DROP = c(0,10), STOP = 18 ) ), 
			msg = "STOP must be a logical" )
	
	checkEquals( interimAnalysis( mtcars ), list(), 
			msg = "empty `interimCode`")
	
	checkEquals( interimAnalysis( mtcars , NULL), list(), 
			msg = "NULL `interimCode`")
	
	myData <- data.frame(DOSE=c(0, 15, 30), TEST = 1:3)
	
	myFun <- function(data) {
		outList <- list()
		outList$STOP <- any(data$TEST) > 5
		myTest <- data$TEST > 2
		if (any(myTest)) outList$DROP <- data$DOSE[myTest]
		outList
	}
	
	checkEquals( interimAnalysis(myData, myFun), list(STOP = FALSE, DROP = 30) )
	
	myData <- data.frame(DOSE=c(0, 15, 30), TEST = 1:3)
	
	myFun <- function(data) {
		outList <- list()
		outList$STOP <- any(data$TEST) > 5
		myTest <- data$TEST > 2
		if (any(myTest)) outList$KEEP <- myTest
		outList
	}
	
	checkEquals( interimAnalysis(myData, myFun), list(STOP = FALSE, KEEP = c(F, F, T)) )
	
}
