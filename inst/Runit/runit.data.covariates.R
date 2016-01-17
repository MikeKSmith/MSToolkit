if( !exists("unitTestPath")) unitTestPath <- system.file(package = "MSToolkit", "Runit")
covariates.datapath <- file.path( unitTestPath , "data", "createCovariates" )
cat("covariates.datapath: ", covariates.datapath , "\n")

test.data.covariates.cont <- function(){
  
  # wrong mean 
  checkException( createContinuousCovariates( 10, mean = "a,b"  ) )
 
  # wrong subjects 
  checkException( createContinuousCovariates( -10, mean = "0,1"  ) )

  # wrong covariance
  checkException( createContinuousCovariates( 10, mean = "0,1", covariance = "1,1,1,1,1,1"  ) )
  
  # dimension problem
  checkException( createContinuousCovariates( 10, mean = "0,1", names = "b" ) )
  
  # duplicated names
  checkException( createContinuousCovariates( 10, mean = "0,1", names = c("X", "X")  ) )
  
  # wrong names
  checkException( createContinuousCovariates( 10, mean = "0,1", names = c("X", ".23")  ) )
  
  # digits > 0
  checkException( createContinuousCovariates( 10, mean = "0,1", names = c("X", "Y"), digits = -1  ) )
  
  # maxDraws > 0
  checkException( createContinuousCovariates( 10, mean = "0,1", names = c("X", "Y"), maxDraws = -100  ) )
  
  # wrong `idCol`
  checkException( createContinuousCovariates( 10, mean = "0,1", names = c("X", "Y"), idCol = ".534"  ) )
  
  dat <- createContinuousCovariates( 10, mean = "0,1", names = c("X", "Y")  )
  checkEquals( nrow(dat), 10 )
  checkEquals( ncol(dat), 3  ) # SUBJ, X, Y
  checkEquals( names(dat), c("SUBJ", "X", "Y")  ) # SUBJ, X, Y    
    
}

test.data.covariates.disc <- function(){
  
  dat <- createDiscreteCovariates( 10 , names = "X", probs = ".1,.9", values = "1,2")
  checkEquals( nrow(dat), 10 )
  checkTrue( all( dat[,2] %in% 1:2 ) )
  
  # wrong names
  checkException( createDiscreteCovariates( 10 , probs = ".1,.9", values = "1,2", names = "43") )
  checkException( createDiscreteCovariates( 10 , probs = ".1,.9", values = "1,2", idCol = "43") )
  
  # dimension problem
  checkException( createDiscreteCovariates( 100 , probs = ".1,.9#.3,.3,.4", values = "1,2#1,3" ) )
  checkException( createDiscreteCovariates( 100 , probs = ".1,.9#.3,4", values = "1,2#1,3", names = c("F1", "F2")) ) 
  checkException( createDiscreteCovariates( 100 , probs = ".1,.9#1#1", values = "1,2#1,3", names = c("F1", "F2")) ) 
  
  pa <- data.frame( F1 = rep(0:1, 3), F2 = rep(1:3, each = 2), PROB = c(.1,.2,.1,.2,.2,8) )
  checkException( createDiscreteCovariates( 100 , probArray = pa) ) 
  
  # testing the probArray thing
  padf <- data.frame( 
    F1 = rep(0:1, 3), 
    F2 = rep(1:3, each = 2), 
    PROB = c(.1,.2,.1,.2,.2,.2) )
  paArr <- rbind( c(.1,.1,.2), c(.2,.2,.2) )
  outDf  <- createDiscreteCovariates( 100 , probArray = padf, seed = 10 )  
  outArr <- createDiscreteCovariates( 100 , values = list(0:1, 1:3), probArray = paArr,
    names = "F1,F2", seed = 10)  
  checkEquals( outDf, outArr, 
    msg = "checking the prob array handling")  
    
  checkException( 
    createDiscreteCovariates(5, names="D1,D2", 
    values = "1,2#1,2,3", 
    probArray = rbind(c(.1, .1, .3), c(.3, 8, .1))), 
    msg = "check reject proba array as an array and does not sum up to one" )

  out <- createDiscreteCovariates(5, names="D1,D2", 
    values = "1,2#1,2,3", probArray = rbind(c(0,0,0), c(0,0,1)))
    
  checkTrue( all(out$D1 == 2 & out$D3 == 3), 
    msg = "checking 0 probabilities" )
}

test.data.covariates.ext <- function(){
   
  checkException( createExternalCovariates( 20, names = "X", file = "thisDoesNotExists.csv" ), 
    msg = "Unexisting file generates error" )
    
  checkException( createExternalCovariates( 20, names = "X1,X2,X3", 
    file =  "wrongTestCovariates.csv", workingPath = covariates.datapath ),
    msg = "Not correctly formatted csv file generates error" )
    
  testFile <- "testCovariates.csv"  
  checkException( createExternalCovariates( 20, names = "YY", 
    file = testFile, workingPath = covariates.datapath ), 
    msg = "Unfound variables in the file generates error" )
  
  checkException( createExternalCovariates( 20, names = "X1",
    file = testFile, dataId = "SUBJECTS", workingPath = covariates.datapath ), 
    msg = "Unfound `dataId` in the file generates error" )
    
  checkException( createExternalCovariates( 20, names = "X1", 
  file = testFile, refCol = "SUBJECTS", workingPath = covariates.datapath ), 
    msg = "Unfound `refCol` in the file generates error" )

  checkException( createExternalCovariates( 20, names = ".25352" ), 
    msg = "Invalid `names` generates an error" )
    
  checkException( createExternalCovariates( 20, names = "X1,X1" ), 
    msg = "Duplicated `names` generates an error" )
   
  checkException( createExternalCovariates( 20, names = "X1", dataId = ".43gt4e" ), 
    msg = "Wrong `dataId` generates an error" )
    
  checkException( createExternalCovariates( 20, names = "X1", dataId = ".43gt4e" ), 
    msg = "Wrong `refCol` generates an error" )

  checkException( createExternalCovariates( 20, names = "X1", idCol = ".43fewfgt4e" ), 
    msg = "Wrong `dataId` generates an error" )
      
    
  ## subset checks  
  checkException( createExternalCovariates( 20, names = "X1", subset = "1<X1<2<4" ), 
    msg = "Incorrect subset code (Too many comparators) generates error" )
    
  checkException( createExternalCovariates( 20, names = "X1", subset = "X1" ), 
    msg = "Incorrect subset code (Too few comparators) generates error" )
  
  checkException( createExternalCovariates( 20, names = "X1", subset = "-1202@{} > 1" ), 
    msg = "Incorrect subset code generates error" )
    
  checkException( createExternalCovariates( 20, names = "X1", subset = "X1 >" ), 
    msg = "Incorrect subset code (Empty side) generates error" )
      
  checkException( 
    createExternalCovariates( 20, names = "X1", 
      file = testFile, subset = "YY > 4", dataId = "ID", workingPath = covariates.datapath ), 
    msg = "subset on unexisting variables generates an error" )
    
  checkException( 
    createExternalCovariates( 20, names = "X1", subset = "X1 > 100", dataId="ID",
      file = testFile, workingPath = covariates.datapath ), 
    msg = "percent must be lower than 100" )
  
  checkTrue( 
    all( createExternalCovariates( 20, names = "X1", subset = "X1 > 0",
      file = testFile, dataId = "ID", workingPath = covariates.datapath )$X1 > 0 ), 
    msg = "subset correctly applied" )
   
  dat <- createExternalCovariates( 20, names = "X1", subset = ".7 < X1 < .8", dataId = "ID",
      file = testFile, workingPath = covariates.datapath )$X1
  checkTrue( all( dat > .7 & dat < .8), 
    msg = "subset correctly applied" )
    
  dat <- createExternalCovariates( 20, names = "X1", dataId = "ID", 
    subset = c(".7 < X1 < .8", "-1 <= X2 <= 1"),  
    file = testFile, workingPath = covariates.datapath )
  checkTrue( all( dat$X1 > .7 & dat$X1 < .8 & dat$X2 >= -1 & dat$X2 <= 1), 
    msg = "subset correctly applied" )

  checkException( createExternalCovariates( 20, names = "X1", sameRow = FALSE, refCol = "ID",  
    dataId = "ID", file = testFile, workingPath = covariates.datapath ) , 
    msg = "checking incompatibility between refCol and sameRow")
    
  # about the percent argument
  checkException( 
    createExternalCovariates( 20, names = "X1", dataId="ID",
      file = testFile, workingPath = covariates.datapath, percent = "x" ), 
    msg = "percent can't be converted to a number" )
  
  checkException( 
    createExternalCovariates( 20, names = "X1", dataId="ID",
      file = testFile, workingPath = covariates.datapath, percent = "10,20" ), 
    msg = "percent must be of length 1" )
    
  checkException( 
    createExternalCovariates( 20, names = "X1", dataId="ID",
      file = testFile, workingPath = covariates.datapath, percent = "-10" ), 
    msg = "percent must be greater than 0" )
    
  checkException( 
    createExternalCovariates( 20, names = "X1", dataId="ID",
      file = testFile, workingPath = covariates.datapath, percent = "1910" ), 
    msg = "percent must be lower than 100" )

  testSameRowFile <- "testSameRow.csv" 
  # the imported dataset has two columns X1 and X2 which have the same values
  dataSameRow <- createExternalCovariates( 20, names = "X,Y",dataId="ID",
    file = testSameRowFile, sameRow = TRUE, workingPath = covariates.datapath )[,-1] 
  checkTrue( all( apply( dataSameRow, 1, diff ) == 0 ), 
    msg = "checking the sameRow functionality")

  # checks on the outputs
  out <- createExternalCovariates( 50, names = "X1,X2", dataId="ID",
      file = testFile, workingPath = covariates.datapath )
  checkEquals( nrow(out), 50, 
    msg = "Checking the number of rows of the output")
  
  checkTrue( all( names(out) %in% c("X1","X2","SUBJ")   ), 
    msg = "Checking the naming of columns" )    
  
  out <- createExternalCovariates( 50, names = "X1,X2", 
      file = testFile, workingPath = covariates.datapath,
      idCol = "SUB", dataId = "ID")  
  checkTrue( all( names(out) %in% c("X1","X2","SUB")   ), 
    msg = "Checking the naming of columns" )    
  
  out <- createExternalCovariates( 50, names = "X1,X2", 
      file = testFile, workingPath = covariates.datapath, 
      idCol = "SUB", dataId = "ID", refCol = "ID")  
  checkTrue( all( names(out) %in% c("X1","X2","SUB", "ID.refCol")   ), 
    msg = "Checking the naming of columns" )    
    
  out <- createExternalCovariates( 50, names = "X1,X2", 
      file = testFile, workingPath = covariates.datapath, 
      idCol = "SUB", dataId = "ID", refCol = "ID", refColSuffix = "")  
  checkTrue( all( names(out) %in% c("X1","X2","SUB", "ID.")   ), 
    msg = "Checking the naming of columns" )    
   
  # test the seed
  out1 <- createExternalCovariates( 50, names = "X1,X2", dataId="ID",
      file = testFile, workingPath = covariates.datapath , seed = 10)
  rnorm( 1002 )
  out2 <- createExternalCovariates( 50, names = "X1,X2", dataId="ID",
      file = testFile, workingPath = covariates.datapath , seed = 10)
  checkTrue( identical( out1, out2 ) )

  
}


test.data.covariates.wrapper <- function(){
    testFile <- "testCovariates.csv"

  checkException( createCovariates( subjects = -3 ), 
    msg = "wrong subjects")

  checkException( createCovariates( subjects = 10, idCol = "ID, SUB" ), 
    msg = "id too long")
    
  checkException( createCovariates( subjects = 10, idCol = "4542" ), 
    msg = "invalid ID")
  
  checkTrue( all(createCovariates( subjects = 100 ) == 1:100 ), 
    msg= "test when no covariates")
    
  checkEquals( names( createCovariates( subjects = 100 , idCol = "SUB" )) , "SUB", 
    msg= "test idCol")
    

  # names incompatibility
  checkException( createCovariates( 30, conNames = "X1,X2", extNames = "X2, X3", disNames = "X4, X5" ), 
    msg = "incompatibility in names" )  
    
  # the continuous alone
  d1 <- createCovariates( 30, conNames = "X,Y", conMean = "0,0" , seed = 10)  
  d2 <- createContinuousCovariates( 30, names = "X,Y", mean = "0,0", seed = 10 )
  checkEquals( d1, d2 , 
    msg = "simple check only continuous covariates")

  d1 <- createCovariates( 30, conNames = "X,Y", conMean = "0,0" , conCov = "1,0,1", seed = 10)  
  d2 <- createContinuousCovariates( 30, names = "X,Y", mean = "0,0", covariance = "1,0,1", seed = 10 )
  checkEquals( d1, d2 , 
    msg = "simple check only continuous covariates, using cov matrix")
  
  d1 <- createCovariates( 30, conNames = "X,Y", conMean = "0,0" , conCov = "1,0,1", seed = 10, conRange = "-1<X<1")  
  d2 <- createContinuousCovariates( 30, names = "X,Y", mean = "0,0", covariance = "1,0,1", seed = 10 , range = "-1<X<1")
  checkEquals( d1, d2 , 
    msg = "simple check only continuous covariates, with range")
    
  # discrete alone
  d1 <- createCovariates( 70, disNames = "P1,P2", disValues = "1,2#3,5,6" , disProbs = ".5,.5#.3,.3,.4" , seed = 10)  
  d2 <- createDiscreteCovariates( 70, names = "P1,P2", values = "1,2#3,5,6" , probs = ".5,.5#.3,.3,.4" , seed = 10)
  checkEquals( d1, d2 , 
    msg = "simple check only discrete covariates")
  
  # external alone
  d1 <- createExternalCovariates( 80, names = "X1", dataId="ID",
    subset = c(".7 < X1 < .8", "-1 <= X2 <= 1"),  seed = 3, 
    file = testFile, workingPath = covariates.datapath )  
  d2 <- createCovariates( 80, extNames = "X1", extSubset = c(".7 < X1 < .8", "-1 <= X2 <= 1"), 
    extFile = testFile , extDataId="ID", workingPath = covariates.datapath , seed = 3)  
  checkEquals( d1, d2 , 
    msg = "simple check only external covariates")
    
  # test altogether
  dAll <- createCovariates( 30, conNames = "X,Y", conMean = "0,0" , conCov = "1,0,1", 
    seed = 10, conRange = "-1<X<1", disNames = "P1,P2", disValues = "1,2#3,5,6" , 
    disProbs = ".5,.5#.3,.3,.4", extNames = "X1",
    extDataId="ID", extFile = testFile, workingPath = covariates.datapath )  
  dCon <- createContinuousCovariates( 30, names = "X,Y", mean = "0,0", covariance = "1,0,1", seed = 10 , range = "-1<X<1")
  dDis <- createDiscreteCovariates( 30, names = "P1,P2", values = "1,2#3,5,6" , probs = ".5,.5#.3,.3,.4" , seed = 10)
  dExt <- createExternalCovariates( 30, names = "X1", dataId="ID",
    file = testFile, workingPath = covariates.datapath, seed = 10 )  
  checkTrue( identical( dAll[, c("SUBJ", "X", "Y")], dCon ), 
    msg = "check altogether 1" )  
  checkTrue( identical( dAll[, c("SUBJ", "P1", "P2")], dDis ), 
    msg = "check altogether 2" )  
  checkTrue( identical( dAll[, c("SUBJ", "X1")], dExt ), 
    msg = "check altogether 3" )  
    
    
}

# tests added to comply with issue 3 of sourceforge
# Tue Jul 24 09:08:08 BST 2007 @380 /Internet Time/
test.data.covariates.sf3 <- function() {
  # test the rounding
  checkException( createContinuousCovariates( 10, mean = "100,100,100", names = c("X", "Y", "Z"), digits = "2,3"  ), 
    msg = "digits should have the right length" )
  checkException( createContinuousCovariates( 10, mean = "100,100,100", names = c("X", "Y", "Z"), digits = "2,3,-2"  ), 
    msg = "no negative digits" )
  out <- createContinuousCovariates( 10, mean = "100,100,100", names = c("X", "Y", "Z"), digits = "2,3,2"  )
  checkEquals( out[,2] , round(out[,2], 2), 
    msg = "check the use of a digits vector")
  checkEquals( out[,3] , round(out[,3], 3), 
    msg = "check the use of a digits vector (2)")
  checkEquals( out[,4] , round(out[,4], 2), 
    msg = "check the use of a digits vector (3)")
  out <- createContinuousCovariates( 10, mean = "100,100,100", names = c("X", "Y", "Z"), digits = "3"  )
  checkEquals( out[,2:4] , round(out[,2:4], 3), 
    msg = "check the use of a digits not vector")
}

# tests added to comply with the bug of createDiscreteCovariates
test.data.covariates.disc.debug <- function() {
	
	subjects = 10
	names = "X, Y, Z"
	
	values1 = "1,2#7,8,9#a,b"
	probs1 = ".1,.9#.5,.4,.1#.5,.5"
	
	values2 = c("1,2", "7,8,9", "a,b")
	probs2 = c(".1,.9", ".5,.4,.1", ".5,.5")
	
	values3 = list(c(1, 2), c(7, 8, 9), c("a", "b"))
	probs3 = list(c(.1, .9), c(.5, .4, .1), c(.5, .5))
	
	pArray1 <- data.frame(expand.grid( X = 1:2, Y = 7:9, Z = c("a", "b")), PROB = c(rep(0.08, 10), 0.1, 0.1))
	pArray2 <- array(c(rep(0.08, 10), 0.1, 0.1), dim = c(2, 3, 2))
	pArray3 <- array(c(rep(0.08, 10), 0.1, 0.1), dim = c(2, 3, 2), dimnames = list(c(1, 2), c(7, 8, 9), c("a", "b")))
	
	dat1 <- createDiscreteCovariates(subjects = subjects, names = names, values = values1, probs = probs1)
	dat2 <- createDiscreteCovariates(subjects = subjects, names = names, values = values2, probs = probs2)
	dat3 <- createDiscreteCovariates(subjects = subjects, names = names, values = values3, probs = probs3)
	
	dat4 <- createDiscreteCovariates(10, probArray = pArray1)
	#dat5 <- createDiscreteCovariates(10, names = "X, Y, Z", values = list(c(1, 2), c(7, 8, 9), c("a", "b")), probArray = pArray2)
	dat6 <- createDiscreteCovariates(10, names = "X, Y, Z", probArray = pArray3)
	
	checkTrue(is.data.frame(dat4))
	checkTrue(is.data.frame(dat6))
	
}


test.data.covariates.disc.handleProbArray <- function() {
	
	# 2 vars
	values0 <- list(c(7, 8, 9), c("a", "b"))
	probs0 <- list(c(.5, .4, .1), c(.5, .5))
	names(values0) <- c("Y", "Z")
	names(probs0) <- c("Y", "Z")
	p0 <- c(0.25, 0.2, 0.05, 0.25, 0.2, 0.05)
	
	pArray1 <- data.frame(expand.grid(a = 7:9, b = c("a", "b")), PROB = p0)
	pArray2 <- array(p0, dim = c(3, 2))
	pArray3 <- array(p0, dim = c(3, 2), dimnames = list(c(7, 8, 9), c("a", "b")))
	
	grid1 <- MSToolkit:::.handleProbArray(values = values0, probs = probs0)
	grid2 <- MSToolkit:::.handleProbArray(probArray = pArray1, values = values0)
	grid3 <- MSToolkit:::.handleProbArray(probArray = pArray2, values = values0)
	grid4 <- MSToolkit:::.handleProbArray(probArray = pArray3, values = values0)
	
	#checkEquals(grid1, grid2)
	checkEquals(grid1, grid3)
	checkEquals(grid1, grid4)
	
	# 3 vars
	values0 <- list(c(1, 2), c(7, 8, 9), c("a", "b"))
	probs0 <- list(c(.1, .9), c(.5, .4, .1), c(.5, .5))
	names(values0) <- c("X", "Y", "Z")
	names(probs0) <- c("X", "Y", "Z")
	p0 <- c(0.025, 0.225, 0.020, 0.180, 0.005, 0.045, 0.025, 0.225, 0.020, 0.180, 0.005, 0.045)
	
	pArray1 <- data.frame(expand.grid( X = 1:2, Y = 7:9, Z = c("a", "b")), PROB = p0)
	pArray2 <- array(p0, dim = c(2, 3, 2))
	pArray3 <- array(p0, dim = c(2, 3, 2), dimnames = list(c(1, 2), c(7, 8, 9), c("a", "b")))
	
	grid1 <- MSToolkit:::.handleProbArray(values = values0, probs = probs0)
	grid2 <- MSToolkit:::.handleProbArray(probArray = pArray1, values = values0)
	grid3 <- MSToolkit:::.handleProbArray(probArray = pArray2, values = values0)
	grid4 <- MSToolkit:::.handleProbArray(probArray = pArray3, values = values0)
	
	checkEquals(grid1, grid3)
	checkEquals(grid1, grid4)
}


test.data.covariates.timevarying <- function() {
	subjects <- 1:10
	names <- "X, Y, Z"
	mean <- list(X = 1:4, Y = rep(3, 4), Z = "2.5, 3, 3.2, 3.6")
	covariance = list(1, 2:5, cbind(c(1,.5,.3,0), c(.5,1,0,0), c(.3,0,1,0), c(0,0,0,1)))
	range = list("10>=X>0", NULL, c("Z>0", "Z<=10"))
	digits = 2
	maxDraws = 100
	seed = 99
	idCol = "SUBJ"
	timeCol = "TIME"
	treatPeriod = c(0.25, 0.5, 1, 12)
	
	dat <- createTimeVaryingCovariates(10, "X, Y, Z", 
			mean <- list(X = 1:4, Y = rep(3, 4), Z = "2.5, 3, 3.2, 3.6"),
			covariance = list(1, 2:5, cbind(c(1,.5,.3,0), c(.5,1,0,0), c(.3,0,1,0), c(0,0,0,1))),
			range = list("10>=X>0", NULL, c("Z>0", "Z<=10")),
			idCol = "SUBJ", timeCol = "TIME", treatPeriod = c(0.25, 0.5, 1, 12))
	
	checkEquals(unique(dat$TIME), c(0.25, 0.5, 1, 12))
}


