if( !exists("unitTestPath")) unitTestPath <- system.file(package = "MSToolkit", "Runit")
testDir <- file.path( unitTestPath, "testdata.datastorage" )

# Date Jun 30 2007
# Author: Francisco
test.createDirectories.CSV <- function() {
  setEctdDataMethod("CSV")
  dir.create( tempDir <- tempfile() )
  
  x <- createDirectories(workingPath = tempDir)
  checkTrue(all(x), msg = "all of the directories should be created")
  checkTrue(all(file.exists(file.path(tempDir, c("ReplicateData", "MacroEvaluation", "MicroEvaluation")))))
  
  x <- createDirectories(dirNames = c("Rep", "MacroEvaluation", "Micro"), workingPath = tempDir)
  checkTrue(!any(x))
  checkException(createDirectories(dirName = c(), workingPath = tempDir))

  checkException( createDirectories(dirNames = letters, workingPath = tempDir) )
  try(unlink(tempDir, recursive = TRUE))
}

# Date: Jul 1 2007
# Author: Francisco

test.removeDirectories.CSV <- function()
{
	setEctdDataMethod("CSV")
	tempDir <- file.path(tempdir(), "DataStorageTemp")
  if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
checkTrue(dir.create(tempDir))
  createDirectories(workingPath = tempDir )
  
  # createDirectories()
  removeDirectories(workingPath = tempDir )
  checkTrue(length(dir(path = tempDir)) == 0)
  
  createDirectories(dirNames = c("Rep", "Micro", "Macro"), workingPath = tempDir)
  removeDirectories(dirNames = c("Rep", "Micro", "Macro"), workingPath = tempDir)
  
  checkTrue(length(dir(path = tempDir)) ==0 )
  createDirectories(dirNames = c("ReplicateData", "MacroEvaluation"), workingPath = tempDir)
  removeDirectories(dirNames = "ReplicateData", workingPath = tempDir)
  checkTrue(length(dir(path=tempDir))==1)
  unlink(tempDir, recursive = TRUE)

}

test.createDirectories.RData <- function() {
	setEctdDataMethod("RData")
	dir.create( tempDir <- tempfile() )
	
	x <- createDirectories(workingPath = tempDir)
	checkTrue(all(x), msg = "all of the directories should be created")
	checkTrue(all(file.exists(file.path(tempDir, c("ReplicateData", "MacroEvaluation", "MicroEvaluation")))))
	
	x <- createDirectories(dirNames = c("Rep", "MacroEvaluation", "Micro"), workingPath = tempDir)
	checkTrue(!any(x))
	checkException(createDirectories(dirName = c(), workingPath = tempDir))
	
	checkException( createDirectories(dirNames = letters, workingPath = tempDir) )
	try(unlink(tempDir, recursive = TRUE))
}

test.removeDirectories.RData <- function()
{
	setEctdDataMethod("RData")
	tempDir <- file.path(tempdir(), "DataStorageTemp")

	if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
	checkTrue(dir.create(tempDir))
	createDirectories(workingPath = tempDir )
	
	# createDirectories()
	removeDirectories(workingPath = tempDir )
	checkTrue(length(dir(path = tempDir)) == 0)
	
	createDirectories(dirNames = c("Rep", "Micro", "Macro"), workingPath = tempDir)
	removeDirectories(dirNames = c("Rep", "Micro", "Macro"), workingPath = tempDir)
	
	checkTrue(length(dir(path = tempDir)) ==0 )
	createDirectories(dirNames = c("ReplicateData", "MacroEvaluation"), workingPath = tempDir)
	removeDirectories(dirNames = "ReplicateData", workingPath = tempDir)
	checkTrue(length(dir(path=tempDir))==1)
	unlink(tempDir, recursive = TRUE)
	
}

# Author: Francisco
# Date: Jul 3 2007

test.readData.CSV <- function()
{   
  setEctdDataMethod("CSV")
  x <- read.csv(paste(testDir,"/microSummary.csv", sep=""))
  y <- readData(dataType = "Micro", dataNumber = 1, workingPath = testDir)
  checkTrue(identical(x[1:10,-c(1,2,13)], y[,-11]))
  checkException(readData(dataType = "Micro", dataNumber = 2, workingPath = testDir))
  checkTrue(!identical(x[2:11,],readData(dataType = "Micro", dataNumber = 1, workingPath = testDir)))
  checkException(readData(dataType = "microeval", dataNumber =1))

  x <- read.csv(paste(testDir,"/ReplicateSample.csv", sep=""))
  x1 <- x[20:80, ]
  rownames(x1) <- 1:61 
  y <- readData(dataType = "Replicate", dataNumber = 10, workingPath = testDir)
  checkException(readData(dataType = "Replicate", dataNumber = 11, workingPath = testDir))
                                 
}

test.writeData.CSV <- function()
{
	setEctdDataMethod("CSV")
	tempDir <- file.path(tempdir(), "DataStorageTemp")
	if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
	checkTrue(dir.create(tempDir))

	x <- read.csv(paste(testDir,"/ReplicateSample.csv",sep=""))[20:30,]
	createDirectories("ReplicateData", workingPath = tempDir)
	writeData(x, dataNumber = 1001, dataType = "Replicate", workingPath = tempDir)
	checkTrue(file.exists(writtenFile <- paste(tempDir, "/ReplicateData/replicate1001.csv", sep="")))
  unlink(tempDir, recursive = TRUE)
}


test.readAllData.CSV <- function(){
	setEctdDataMethod("CSV")
	tempDir <- file.path(tempdir(), "DataStorageTemp")
	if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
	checkTrue(dir.create(tempDir))
	microData <- read.csv(file.path(testDir, "microSummary.csv")) [1:12,-c(2,13)]
  createDirectories("MicroEvaluation", workingPath = tempDir)
 
	writeData(microData[1:3,], dataNumber = 1, dataType = "Micro", workingPath=tempDir)
	writeData(microData[4:6,], dataNumber = 2, dataType = "Micro", workingPath=tempDir)
	writeData(microData[7:9,], dataNumber = 3, dataType = "Micro", workingPath=tempDir)
	writeData(microData[10:12,], dataNumber = 4, dataType = "Micro", workingPath=tempDir)
	x <- readAllData(dataType = "Micro", workingPath = tempDir)

  rownames(microData)  <- rownames(x)
  checkEquals(x[,-1], microData, msg = "checking the readAllData function with MicroEvaluation data") 	
  checkTrue(all(x$Replicate == rep(1:4, each=3)), msg = "Check subset replicate variable created correctly")
	
  # Now check it with a subset of data
	x <- readAllData(dataType = "Micro", workingPath = tempDir, replicates = 2:3)
  
  y <- microData[4:9, ]
  rownames(y) <- rownames(x)
  checkEquals(x[,-1], y, msg = "checking the readAllData function with a reading of partial MicroEvaluation data")
  checkTrue(all(x$Replicate == rep(2:3, each=3)), msg = "Check subset replicate variable created correctly")
  unlink(tempDir, recursive = TRUE)
 
}

test.readData.RData <- function()
{   
	nowMethod <- getEctdDataMethod()
	tempDir <- file.path(tempdir(), "DataStorageTemp")
	if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
	checkTrue(dir.create(tempDir))
	resetEctdColNames()
	setEctdDataMethod("RData")
	generateData(3, 5, respEqn = "DOSE", workingPath = tempDir, treatDoses = 1:2, idCol = "RDATATEST")
	resetEctdColNames()
	genFiles <- list.files(file.path(tempDir, "ReplicateData"))
	checkTrue(all(genFiles == paste("replicate000", 1:3, ".RData", sep="")))
	x <- readData(1, workingPath = tempDir)
	checkTrue(nrow(x) == 5)
	checkTrue("RDATATEST" %in% names(x))
	x <- readAllData(workingPath = tempDir)
	checkTrue(nrow(x) == 3*5)
	checkTrue("RDATATEST" %in% names(x))
	if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
	setEctdDataMethod(nowMethod)
	
}

test.readData.Internal <- function()
{
	nowMethod <- getEctdDataMethod()
	.ectdEnv$DataStore <- NULL
	setEctdDataMethod("Internal")
	resetEctdColNames()	
	generateData(3, 5, respEqn = "DOSE", treatDoses = 1:2, idCol = "INTERNALTEST")
	resetEctdColNames()
	checkTrue(length(.ectdEnv$DataStore) == 3)
	checkTrue(all(sapply(.ectdEnv$DataStore, is.data.frame)))
	checkTrue(all(sapply(.ectdEnv$DataStore, nrow) == 5))
	x <- readData(1)
	checkTrue(nrow(x) == 5)
	checkTrue("INTERNALTEST" %in% names(x))
	x <- readAllData()
	checkTrue(nrow(x) == 3*5)
	checkTrue("INTERNALTEST" %in% names(x))
	.ectdEnv$DataStore <- NULL
	setEctdDataMethod(nowMethod)
	
}

test.writeData.RData <- function()
{
	nowMethod <- getEctdDataMethod()
	setEctdDataMethod("RData")
	tempDir <- file.path(tempdir(), "DataStorageTemp")
	if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
	checkTrue(dir.create(tempDir))
	myDf <- data.frame(X = 1:5, Y = 5:1)
	checkTrue(all(createDirectories(workingPath = tempDir)))
	writeData(myDf, 1, "ReplicateData", workingPath = tempDir)
	writeData(myDf, 1, "Micro", workingPath = tempDir)
	writeData(myDf, 1, "Macro", workingPath = tempDir)
	checkTrue(list.files(file.path(tempDir,"ReplicateData")) == "replicate0001.RData")
	checkTrue(list.files(file.path(tempDir,"MicroEvaluation")) == "micro0001.csv")
	checkTrue(list.files(file.path(tempDir,"MacroEvaluation")) == "macro0001.csv")
	inData <- readData(1, workingPath = tempDir)
	checkTrue(identical(inData, myDf))
	setEctdDataMethod(nowMethod)
}

test.writeData.Internal <- function()
{
	nowMethod <- getEctdDataMethod()
	setEctdDataMethod("Internal")
	.ectdEnv$DataStore <- NULL
	myDf <- data.frame(X = 1:5, Y = 5:1)
	writeData(myDf, 1, "ReplicateData")
	checkTrue(length(.ectdEnv$DataStore) == 1)
	checkTrue(identical(.ectdEnv$DataStore[[1]], myDf))
	.ectdEnv$DataStore <- NULL
	setEctdDataMethod(nowMethod)
}

# SF issue 6
# Tue Jul 24 12:35:40 BST 2007 @524 /Internet Time/
test.readnonmemdata.sf6 <- function( ){
  if("MSToolkit" %in% search() ){ # need that because .readAndCheckInputFile is internal
    readFun <- MSToolkit:::.readAndCheckInputFile
    
    nohData <- readFun( file.path( unitTestPath , "testdata.datastorage","NONMEMDataNoHeader.fit"  ) )
    heaData <- readFun( file.path( unitTestPath , "testdata.datastorage","NONMEMDataWithHeader.fit") )
    csvData <- readFun( file.path( unitTestPath , "testdata.datastorage","NONMEMcsv.csv"           ) )
   
    checkEquals( nohData, heaData,  msg = "check import of NONMEM data (1)" )
    checkEquals( nohData, csvData,  msg = "check import of NONMEM data (2)" )
    
    checkEquals( nrow(nohData), 10, msg = "check import of NONMEM data (3)" )
    checkEquals( nrow(heaData), 10, msg = "check import of NONMEM data (4)" )
    checkEquals( nrow(csvData), 10, msg = "check import of NONMEM data (5)" )
    
    
  }
  
}

test.getReplicates <- function() {

	# Set test directory
	tempDir <- file.path(tempdir(), "getRepTest")
	if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
	checkTrue(dir.create(tempDir))
	checkTrue(dir.create(file.path(tempDir, "TestDir")))
	cat("Hello", file = file.path(tempDir, "TestDir", "test0001.csv"))
	cat("Hello", file = file.path(tempDir, "TestDir", "test0001.RData"))
	nowMethod <- getEctdDataMethod()

	# Exceptions
	setEctdDataMethod("CSV")
	checkException(getReplicates(1))
	checkException(getReplicates(letters))
	checkException(getReplicates(prefix = 1))
	checkException(getReplicates(prefix = LETTERS))
	checkException(getReplicates(method = 1))
	checkException(getReplicates(method = letters))
	checkException(getReplicates(workingPath = 1))
	checkException(getReplicates(workingPath = letters))
	checkException(getReplicates(path = "MADEUPPATH", method = "CSV"))
	checkException(getReplicates(path = "MADEUPPATH", method = "RData"))
	checkException( getReplicates(workingPath = tempDir))
	checkException(getReplicates("TestDir", workingPath = tempDir))
	checkTrue(getReplicates("TestDir", workingPath = tempDir, prefix = "test") == 1)
	setEctdDataMethod("RData")
	checkException(getReplicates("TestDir", workingPath = tempDir))
	checkTrue(getReplicates("TestDir", workingPath = tempDir, prefix = "test") == 1)

	# CSV Method first
	setEctdDataMethod("CSV")
	if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
	checkTrue(dir.create(tempDir))
	generateData(5, 3, respEqn = "DOSE", workingPath = tempDir, treatDoses = 1)
	checkTrue(all(getReplicates(workingPath = tempDir) == 1:5))
	unlink(file.path(tempDir, "ReplicateData", "replicate0003.csv"))	
	checkTrue(all(getReplicates(workingPath = tempDir) == c(1:2, 4:5)))

	# RData method
	setEctdDataMethod("RData")
	if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
	checkTrue(dir.create(tempDir))
	generateData(5, 3, respEqn = "DOSE", workingPath = tempDir, treatDoses = 1)
	checkTrue(all(getReplicates(workingPath = tempDir) == 1:5))
	unlink(file.path(tempDir, "ReplicateData", "replicate0003.RData"))	
	checkTrue(all(getReplicates(workingPath = tempDir) == c(1:2, 4:5)))
	
	# Internal Method
	setEctdDataMethod("Internal")
	.ectdEnv$DataStore <- NULL
	generateData(5, 3, respEqn = "DOSE", workingPath = tempDir, treatDoses = 1)
	checkTrue(all(getReplicates(workingPath = tempDir) == 1:5))
	.ectdEnv$DataStore <- NULL
	
	# Reset method
	setEctdDataMethod(nowMethod)
	
}
