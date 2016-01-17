if( !exists("unitTestPath")) unitTestPath <- system.file(package = "MSToolkit", "Runit")
testDir <- file.path( unitTestPath, "testdata.supportfunctions" )

# Author: Francisco
# Date: July 16 

test.readAndCheckCsvFile <- function()
{
  if( "MSToolkit" %in% search() ) { 
    testFileName1 <- file.path(testDir,"testdata.checkCsvFile.1.csv")
    testFileName2 <- file.path(testDir,"testdata.checkCsvFile.2.csv")
    testFileName3 <- file.path(testDir,"testdata.checkCsvFile.3.csv")
    testFileName4 <- file.path(testDir,"testdata.checkCsvFile.4.csv")
    
    x <- read.csv(testFileName1)      
    readFun <- MSToolkit:::.readAndCheckInputFile  # because internal functions are not exported
    checkTrue(identical(x, readFun(file = testFileName1, variables = c("INTERIM", "INTERIMC", "DOSE", "MEAN", "SE", "LOWER", "UPPER", "N", "DROPPED", "STOPPED") )), 
      msg = "this csv file should have no problem, hence the data should be returned")    
    checkTrue(identical(x, readFun(file = testFileName4, variables = c("INTERIM", "INTERIMC", "DOSE", "MEAN", "SE", "LOWER", "UPPER", "N", "DROPPED", "STOPPED") )), 
      msg = "the case of the data names is different, but the function should have no problem with this")        
    
    checkException(readFun(file = testFileName2, variables = c("INTERIM","INTERIMC","DOSE","MEAN","SD","LOWER","UPPER","N","DROPPED","STOPPED")), 
      msg = "Some of the information is missing from the last two columns")
    checkException(readFun(file = testFileName3, variables = c("INTERIM", "INTERIMC", "DOSE", "MEAN", "SD", "LOWER", "UPPER", "N", "DROPPED", "STOPPED")), 
      msg = "INTERIM is missing")
    checkException(readFun(file = testFileName1, variables = c("INTERIM", "INTERIMC", "DOSE", "MEAN", "SD", "LOWER", "UPPER", "N", "DROPPED", "STOPPED") ), 
      msg = "Variable SD is missing, is actually called SE") 
  }
}                  
