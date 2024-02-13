if( !exists("unitTestPath")) unitTestPath <- system.file(package = "MSToolkit", "Runit")
parameters.datapath <- file.path( unitTestPath , "data", "createParameters" )
covariates.datapath <- file.path( unitTestPath , "data", "createCovariates" )

test.data.param.ext <- function(){

  testParamFile <- "testParam.csv"
  otherFiles <- paste("testParam", c("nonmem", "spaceDelim", "tabDelim"), sep=".")

  wrongTestParamFile <- "wrongTestParam.csv"
  
  # test the inputs  
  checkException( createExternalParameters( -20 ), 
    msg = "`subjects` must be positive" )
    
  checkException( createExternalParameters( 20, names = ".54325,5432" ), 
    msg = "invalid `names`" )
    
  checkException( createExternalParameters( 20, names = "X,X" ), 
    msg = "duplicated `names`" )
  
  checkException( createExternalParameters( 20, names = "X,Y",idCol = "X" ), 
    msg = "duplicated names between `SUBJ` and `names`" )
  
  checkException( createExternalParameters( 20, names = "X", dataId = ".54325" ), 
    msg = "invalid `dataId`" )
 
  checkException( createExternalParameters( 20, names = "X", dataId = "ID,SUB" ), 
    msg = "`dataId` should be of length one" )
  
  checkException( createExternalParameters( 20, names = "X", idCol = ".54325" ), 
    msg = "invalid `idCol`" )
  
  checkException( createExternalParameters( 20, names = "X", idCol = "ID,SUB" ), 
    msg = "`idCol` should be of length one" )
 
  checkException( createExternalParameters( 20, names = "E0,ED50", 
      file = testParamFile, workingPath = parameters.datapath, 
      betNames = "B1,B1"  ), 
    msg = "Duplicated values in betNames" )
 
  # tests about the file
  checkException( createExternalParameters( 20, names = "E0,ED50,EMAX", 
      file = "doesNotExists.csv", workingPath = parameters.datapath ), 
    msg = "File does not exist" )
  
  checkException( createExternalParameters( 20, names = "E0,ED50,EMAX", 
      file = wrongTestParamFile, workingPath = parameters.datapath ), 
    msg = "wrong formatted file" )
  
  checkException( createExternalParameters( 20, names = "X,Y,ED50", dataId="ID",
      file = testParamFile, workingPath = parameters.datapath ), 
    msg = "missing variables in file" )
    
  checkException( createExternalParameters( 20, names = "E0,ED50", 
      file = testParamFile, workingPath = parameters.datapath, idCol = "SUB" ), 
    msg = "missing ID variables in file" )
    
  # test about errStruc
  checkException( createExternalParameters( 20, names = "E0,ED50", dataId="ID",
      file = testParamFile, workingPath = parameters.datapath, errStruc = "w" ), 
    msg = "wrong errStruc" )

  # Basic check: different delimiters
	dataNone <- createExternalParameters( 20, names = "E0,ED50", dataId="ID",
		file = testParamFile, workingPath = parameters.datapath, 
		errStruc = "None", betNames = "B1,B2", seed = 2 )
	for (i in otherFiles) {
		newData <- createExternalParameters( 20, names = "E0,ED50", dataId="ID",
				file = i, workingPath = parameters.datapath, 
				errStruc = "None", betNames = "B1,B2", seed = 2 )
	
		checkTrue(all(dim(dataNone) == dim(newData)), msg = paste("Check correct dimensions when importing", i))
		checkTrue(all(names(dataNone) == names(newData)), msg = paste("Check correct column names when importing", i))
		checkTrue(all(round(dataNone$ED50, 3) == newData$ED50), msg = paste("Check correct ED50 column when importing", i))
		checkTrue(all(round(dataNone$B1, 3) == newData$B1), msg = paste("Check correct B1 column when importing", i))
	}


  # Basic working
  dataAdd  <- createExternalParameters( 20, names = "E0,ED50", dataId="ID",
    file = testParamFile, workingPath = parameters.datapath, 
    errStruc = "Add", betNames = "B1,B2", seed = 2 )
  dataLogNorm <- createExternalParameters( 20, names = "E0,ED50", dataId="ID",
    file = testParamFile, workingPath = parameters.datapath, 
    errStruc = "Log", betNames = "B1,B2", seed = 2 )
  dataProp <- createExternalParameters( 20, names = "E0,ED50", dataId="ID",
		file = testParamFile, workingPath = parameters.datapath, 
		errStruc = "Pro", betNames = "B1,B2", seed = 2 )

  checkTrue( identical( dataNone$E0   + dataNone$B1, dataAdd$E0  ),  msg = "Testing the Additive errStruc" )
  checkTrue( identical( dataNone$ED50 + dataNone$B2, dataAdd$ED50),  msg = "Testing the Additive errStruc")
  checkTrue( identical( dataNone$E0 * exp(dataNone$B1), dataLogNorm$E0  ), msg = "Testing the Log-Normal errStruc" )
  checkTrue( identical( dataNone$ED50 * exp(dataNone$B2), dataLogNorm$ED50), msg = "Testing the Log-Normal errStruc")
  checkTrue( identical( dataNone$E0 * (1 + dataNone$B1), dataProp$E0  ), msg = "Testing the Proportional errStruc" )
  checkTrue( identical( dataNone$ED50 * (1 + dataNone$B2), dataProp$ED50), msg = "Testing the Proportional errStruc")
    
  dataNone <- createExternalParameters( 20, names = "E0,ED50", dataId="ID",
    file = testParamFile, workingPath = parameters.datapath, 
    errStruc = "N", betNames = "B1,B2", seed = 2 )
  dataAdd  <- createExternalParameters( 20, names = "E0,ED50", dataId="ID",
    file = testParamFile, workingPath = parameters.datapath, 
    errStruc = "A", betNames = "B1,B2", betNums = "2,1", seed = 2 )
  checkTrue( identical( dataNone$E0   + dataNone$B2, dataAdd$E0  ),  msg = "Testing the Additive errStruc and betNums" )
  checkTrue( identical( dataNone$ED50 + dataNone$B1, dataAdd$ED50),  msg = "Testing the Additive errStruc and betNums")
   
  dataNone <- createExternalParameters( 20, names = "E0,ED50,EMAX", dataId="ID",
    file = testParamFile, workingPath = parameters.datapath, 
    errStruc = "N", betNames = "B1,B2", betNums = "1,3", seed = 2 )
  dataAdd  <- createExternalParameters( 20, names = "E0,ED50,EMAX", dataId="ID",
    file = testParamFile, workingPath = parameters.datapath, 
    errStruc = "A", betNames = "B1,B2", betNums = "1,3", seed = 2 )
  dataLogNorm <- createExternalParameters( 20, names = "E0,ED50,EMAX", dataId="ID",
		file = testParamFile, workingPath = parameters.datapath, 
		errStruc = "L", betNames = "B1,B2", betNums = "1,3", seed = 2 )
  dataProp <- createExternalParameters( 20, names = "E0,ED50,EMAX", dataId="ID",
    file = testParamFile, workingPath = parameters.datapath, 
    errStruc = "P", betNames = "B1,B2", betNums = "1,3", seed = 2 )

  checkTrue( identical( dataNone$E0   + dataNone$B1, dataAdd$E0  ),  msg = "Testing the Additive errStruc and betNums" )
  checkTrue( identical( dataNone$ED50, dataAdd$ED50),  msg = "Testing the Additive errStruc and betNums")
  checkTrue( identical( dataNone$EMAX + dataNone$B2, dataAdd$EMAX),  msg = "Testing the Additive errStruc and betNums")
  checkTrue( identical( dataNone$E0 * exp(dataNone$B1), dataLogNorm$E0  ),  msg = "Testing the Log-Normal errStruc and betNums" )
  checkTrue( identical( dataNone$ED50, dataLogNorm$ED50),  msg = "Testing the Log-Normal errStruc and betNums, variable without between should not be exponentiated when errStruc is prop")
  checkTrue( identical( dataNone$EMAX * exp(dataNone$B2), dataLogNorm$EMAX),  msg = "Testing the Log-Normal errStruc and betNums")
  checkTrue( identical( dataNone$E0 * (1 + dataNone$B1), dataProp$E0  ),  msg = "Testing the Proportional errStruc and betNums" )
  checkTrue( identical( dataNone$ED50, dataProp$ED50),  msg = "Testing the Proportional errStruc and betNums, variable without between should not be exponentiated when errStruc is prop")
  checkTrue( identical( dataNone$EMAX * (1 + dataNone$B2), dataProp$EMAX),  msg = "Testing the Proportional errStruc and betNums")
    
   # testing the refCol settings
  checkException( createExternalParameters( 20, names = "E0,ED50,EMAX", dataId="ID",
      file = testParamFile, workingPath = parameters.datapath,refColName = "f09-124" ) , 
      msg = "wrong refColName")
  checkException( createExternalParameters( 20, names = "E0,ED50,EMAX", dataId="ID",
      file = testParamFile, workingPath = parameters.datapath,refColName = "REF" ) , 
      msg = "refColName not in the data")
  checkException( createExternalParameters( 20, names = "E0,ED50,EMAX", dataId="ID",
      file = testParamFile, workingPath = parameters.datapath,refColName = "ID", refCol = "1,2,3" ) , 
      msg = "refColName not in the data")

  
  testCovFile <- "testCovariates.csv"  

  importCov <- createExternalCovariates( 20, names = "X1", dataId="ID",
    file = testCovFile, workingPath = covariates.datapath, refCol = "ID" )
  refcolvalues <- importCov$ID.refCol 
  importPar <- createExternalParameters( 20, names = "B1,B2", dataId="ID",
    file = testParamFile, workingPath = parameters.datapath, refColName = "ID", 
    refCol = refcolvalues )  
  iData <- read.csv( file.path( parameters.datapath, testParamFile) )[, c("ID", "B1", "B2")]  
  for( ref in refcolvalues ){
    checkTrue( all( iData[ iData$ID == ref, "B1" ][1] == importPar[ refcolvalues == ref, "B1"] ) , 
      msg = "Testing the refCol system")
  }
  

  # tests about betNums  
  checkException( createExternalParameters( 20, names = "E0,ED50", dataId="ID",
      file = testParamFile, workingPath = parameters.datapath, 
      betNames = "B1,B2", betNums = "1"  ), 
    msg = "#betNames != #betNums" )
 
  checkException( createExternalParameters( 20, names = "E0,ED50", dataId="ID",
      file = testParamFile, workingPath = parameters.datapath, 
      betNames = "B1,B2", betNums = "a,b"  ), 
    msg = "wrong betNums format" )
 
  checkException( createExternalParameters( 20, names = "E0,ED50,EMAX", dataId="ID",
      file = testParamFile, workingPath = parameters.datapath, 
      betNames = "B1,B2"  ), 
    msg = "Need betNums if index mismatch" )
 
  # tests about subset
  checkException( createExternalParameters( 20, names = "E0,ED50,EMAX", dataId="ID",
      file = testParamFile, workingPath = parameters.datapath, 
      subset = "@fewf-+fw" ), 
    msg = "Non sense code" )
   
   checkException( createExternalParameters( 20, names = "E0,ED50,EMAX", dataId="ID",
      file = testParamFile, workingPath = parameters.datapath, 
      subset = "E0 < 0 < EMAX < 1 " ), 
    msg = "Too many comparators" )
   
   checkException( createExternalParameters( 20, names = "E0,ED50,EMAX", dataId="ID",
      file = testParamFile, workingPath = parameters.datapath, 
      subset = "E0" ), 
    msg = "Too few comparators" )
  
   checkTrue( all( createExternalParameters( 20, names = "E0,ED50,EMAX", dataId="ID",
      file = testParamFile, workingPath = parameters.datapath, 
      subset = "E0 < 0" )$E0 < 0 ), 
      msg = "test that the subset is applied correctly") 
   
   subData <- createExternalParameters( 20, names = "E0,ED50,EMAX", dataId="ID",
      file = testParamFile, workingPath = parameters.datapath, 
      subset = c("E0 < 0", "1 <= B1 <= 2") )
   checkTrue( all( subData$E0 < 0 ), 
      msg = "test that the subset is applied correctly with multiple subsets") 
   checkTrue( all( subData$B1 >= 1 & subData$B1 <= 2  ), 
      msg = "test that the subset is applied correctly with two comparators") 

      
   # seed
  data1  <- createExternalParameters( 20, names = "E0,ED50,EMAX", dataId="ID",
    file = testParamFile, workingPath = parameters.datapath, 
    errStruc = "A", betNames = "B1,B2", betNums = "1,3", seed = 81 )
  rnorm(2100 ) + runif(2)
  data2  <- createExternalParameters( 20, names = "E0,ED50,EMAX", dataId="ID",
    file = testParamFile, workingPath = parameters.datapath, 
    errStruc = "A", betNames = "B1,B2", betNums = "1,3", seed = 81 )
  checkEquals( data1, data2 )
  
}

test.data.param.norm <- function(){
  
  checkException( createNormalParameters( -50 ) , 
    msg = "subjects should be positive")
  
  checkException( createNormalParameters( 50, names = "X, Y" , mean = "0,a") , 
    msg = "wrong mean")
    
  checkException( createNormalParameters( 50, names = "X,X" , mean = "0,a") , 
    msg = "duplicated names")
    
  checkException( createNormalParameters( 50, names = "X,Y" , mean = "0,0,0") , 
    msg = "dimension problem")
    
  checkException( createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,0,0", covariance = "1,0,0,-1,0,-1" ) , 
    msg = "wrong covariance matrix")
  
  checkException( createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,0,0", covariance = "1,0,0,1,0,1,2,3,4" ) , 
    msg = "wrong covariance matrix dims")
   
  checkException( createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,0,0", covariance = "1", maxDraws = -1 ) , 
    msg = "wrong maxDraws value")
    
  checkException( createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,0,0", covariance = "1", maxDraws = 5:6 ) , 
    msg = "wrong maxDraws value")
  
  checkException( createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,0,0", covariance = "1", digits = -1 ) , 
    msg = "wrong digits value")
  
  checkException( createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,0,0", covariance = "1", digits = 1:2 ) , 
    msg = "wrong digits value")
  
  checkException( createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,0,0", covariance = "1", betMean = "0", 
      betNames = ".45", betCov = "1" ) , 
    msg = "wrong betNames value")
    
  checkException( createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,0,0", covariance = "1", betMean = "0", 
      betNames = "R", betCov = "1" ) , 
    msg = "wrong betNames value, not a subset of names")

  checkException( createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,0,0", covariance = "1", betMean = "0", 
      betNames = "X", betCov = "-1" ) , 
    msg = "wrong betCov matrix")

  checkException( createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,0,0", covariance = "1", betMean = "0", 
      betNames = "X", betCov = "1,2,3" ) , 
    msg = "wrong betCov matrix")
 
  checkException( createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,0,0", covariance = "1", betMean = "0", 
      betNames = "X", betCov = "1", range = "]t43t534w-02354" ) , 
    msg = "non sense code")
   
  checkException( createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,0,0", covariance = "1", betMean = "0", 
      betNames = "X", betCov = "1", range = "X" ) , 
    msg = "wrong range code, not enough comparators")    
    
  dataNone <- createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,0,0", covariance = "1", betNames = "X,Y,Z", 
      betCov = "1", betMean = "0,0,0", errStruc = "N", seed = 99 )
  dataAdd  <- createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,0,0", covariance = "1", betNames = "X,Y,Z", 
      betCov = "1", betMean = "0,0,0", errStruc = "A", seed = 99 )
  dataLogNorm <- createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,0,0", covariance = "1", betNames = "X,Y,Z", 
      betCov = "1", betMean = "0,0,0", errStruc = "L", seed = 99 )
      
  errStrucDiff <- round(dataAdd [,c("X", "Y", "Z") ] -  (dataNone[,c("X", "Y", "Z") ] + dataNone[, paste( c("X", "Y", "Z") , ".Between", sep = "") ]), 2) 
  checkTrue( all(unlist(errStrucDiff == 0)) ,  msg = "checking the errStruc" )
  checkTrue( all( dataLogNorm[,c("X", "Y", "Z") ] -  
		dataNone[,c("X", "Y", "Z") ] * exp( dataNone[,paste( c("X", "Y", "Z") , ".Between", sep ="") ] == 0)  )   , 
    msg = "checking the errStruc with None and Log Normal")

dataNone <- createNormalParameters( 50, names = "X,Y,Z" , 
		mean = "0,50,100", covariance = "1", betNames = "X,Y", 
		betCov = "1", betMean = "0,0", errStruc = "None", seed = 99, digits = 10 )
dataNone$Z.Between <- rep(0, nrow(dataNone))
dataAdd  <- createNormalParameters( 50, names = "X,Y,Z" , 
		mean = "0,50,100", covariance = "1", betNames = "X,Y", 
		betCov = "1", betMean = "0,0", errStruc = "Add", seed = 99, digits = 10  )
dataLogNorm <- createNormalParameters( 50, names = "X,Y,Z" , 
		mean = "0,50,100", covariance = "1", betNames = "X,Y", 
		betCov = "1", betMean = "0,0", errStruc = "Log", seed = 99, digits = 10  )
.roundIt <- MSToolkit:::.roundIt

checkTrue( identical( .roundIt(dataAdd [,c("X", "Y") ], 5), .roundIt(dataNone[,c("X", "Y") ] + dataNone[, paste( c("X", "Y"),  ".Between", sep = "") ], 5)  )  , 
		msg = "check the errStruc, not all between, None and Add" )
#checkTrue( all( .roundIt(dataLogNorm[,c("Z", "Y") ] - (dataNone[,c("Z", "Y") ] * exp( dataNone[,paste( c("Z", "Y"), ".Between",sep ="") ])), 2) == 0 ) , 
#		msg = "check the errStruc, not all between, None and Prop, different order")
v1 <- dataLogNorm[,c("Z", "Y") ]
v2 <- dataNone[,c("Z", "Y") ] * exp( dataNone[,paste( c("Z", "Y"), ".Between",sep ="") ])
checkEqualsNumeric(v1, v2, tolerance = 0.1, msg = "\ncheck the errStruc, not all between, None and Prop, different order(1)")

checkTrue( identical( dataAdd$Z , dataNone$Z  )  , msg = "check the errStruc, not between, None and Add" )
checkTrue( identical( dataLogNorm$Z, dataNone$Z  ) , msg = "check the errStruc, not between, None and Prop")  

  dataNone <- createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,50,100", covariance = "1", betNames = "Z,Y", 
      betCov = "1", betMean = "0,0", errStruc = "N", seed = 99, digits = 5 )
  dataAdd  <- createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,50,100", covariance = "1", betNames = "Z,Y", 
      betCov = "1", betMean = "0,0", errStruc = "A", seed = 99, digits = 5 )
  dataLogNorm <- createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,50,100", covariance = "1", betNames = "Z,Y", 
      betCov = "1", betMean = "0,0", errStruc = "L", seed = 99, digits = 5 )

  checkTrue( all( .roundIt(dataAdd [,c("Z", "Y") ]- (dataNone[,c("Z", "Y") ] + dataNone[,paste( c("Z", "Y"), ".Between",sep = "") ]), 3)  ==0) , 
    msg = "check the errStruc, not all between, None and Prop, different order")

#checkTrue( all( .roundIt(dataLogNorm[,c("Z", "Y") ] - (dataNone[,c("Z", "Y") ] * exp( dataNone[,paste( c("Z", "Y"), ".Between",sep ="") ])), 2) == 0 ) , 
#		msg = "check the errStruc, not all between, None and Prop, different order")
v1 <- dataLogNorm[,c("Z", "Y") ]
v2 <- dataNone[,c("Z", "Y") ] * exp( dataNone[,paste( c("Z", "Y"), ".Between",sep ="") ])
checkEqualsNumeric(v1, v2, tolerance = 0.1, msg = "\ncheck the errStruc, not all between, None and Prop, different order(2)")

checkTrue( all( dataAdd$X - dataNone$X  ==0) , 
    msg = "check the errStruc, not between, None and Prop, different order")
  checkTrue( all( dataLogNorm$X - dataNone$X ==0  ) , 
    msg = "check the errStruc, not between, None and Prop, different order")

  # check the outputs
  dataFixed <- createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "0,50,100", covariance = "1" )
  for( va in c("X", "Y", "Z")) {
    checkTrue( all(diff( dataFixed[[va]] ) == 0), msg = "check the fixed parameters")
  }
    
}                      

test.data.param.wrap <- function(){
  testParamFile <- "testParam.csv"
  wrongTestParamFile <- "wrongTestParam.csv"
  
  checkException( createParameters( -10  ), 
    msg = "wrong subject (negative)" )
  
  checkException( createParameters( 10, idCol = "ID, SUB"  ), 
    msg = "wrong id (too long)" )
 
  checkException( createParameters( 10, idCol = "08234ID"  ), 
    msg = "wrong id" )
    
  checkException( createParameters( 10, flagName = "PAROMIT, OMIT"  ), 
    msg = "wrong flagName (too long)" )
 
  checkException( createParameters( 10, idCol = "082PAROMIT"  ), 
    msg = "wrong flagName" )

    
  d1  <- createNormalParameters( 50, names = "X,Y,Z" , 
    mean = "0,0,0", covariance = "1", betNames = "X,Y,Z", 
    betCov = "1", betMean = "0,0,0", errStruc = "A", seed = 99 )
  d2 <-  createParameters( 50, genNames = "X,Y,Z" , 
    genFixedMean = "0,0,0", genFixedCov = "1", genBetweenNames = "X,Y,Z", 
    genBetweenCov = "1", genBetweenMean = "0,0,0", genErrStruc = "A", seed = 99 ) 
  checkTrue( identical( d1, d2 ), 
    msg = "only normal" )  
    
  
  d1  <- createExternalParameters( 20, names = "E0,ED50,EMAX", 
    file = testParamFile, workingPath = parameters.datapath, dataId = "ID",
    errStruc = "A", betNames = "B1,B2", betNums = "1,3", seed = 81 )
  d2  <- createParameters( 20, extNames = "E0,ED50,EMAX", extDataId="ID", 
    extFile = testParamFile, workingPath = parameters.datapath, 
    extErrStruc = "A", extBetween = "B1,B2", extBetweenNums = "1,3", 
    seed = 81 )
  checkTrue( identical( d1, d2 ), 
     msg = "only external" )  
 
  
     
  dNor  <- createNormalParameters( 50, names = "X,Y,Z" , 
    mean = "0,0,0", covariance = "1", betNames = "X,Y,Z", 
    betCov = "1", betMean = "0,0,0", errStruc = "A", seed = 81 )
  dExt  <- createExternalParameters( 50, names = "E0,ED50,EMAX",  dataId="ID",
    file = testParamFile, workingPath = parameters.datapath, 
    errStruc = "A", betNames = "B1,B2", betNums = "1,3", seed = 81 )
  dAll  <- createParameters( 50, extNames = "E0,ED50,EMAX", 
    extFile = testParamFile, workingPath = parameters.datapath, 
    extErrStruc = "A", extBetween = "B1,B2", extBetweenNums = "1,3", extDataId="ID",
    seed = 81, 
    genNames = "X,Y,Z", genFixedMean = "0,0,0", genFixedCov = "1", 
    genBetweenNames = "X,Y,Z", 
    genBetweenCov = "1", genBetweenMean = "0,0,0", genErrStruc = "A" )
  checkTrue( identical( dNor[, c("SUBJ", "X", "Y", "Z") ] , dAll[, c("SUBJ", "X", "Y", "Z")] ) , 
    msg = "all + nor")   
  checkTrue( identical( dExt[,c("SUBJ", "E0", "ED50", "EMAX")], dAll[, c("SUBJ", "E0", "ED50", "EMAX")] ) , 
    msg = "all + ext")   
    
    
  dNor  <- createNormalParameters( 50, names = "X,Y,Z" , 
    mean = "0,0,0", covariance = "1", betNames = "X,Y,Z", 
    betCov = "1", betMean = "0,0,0", errStruc = "A", seed = 81, 
    range = "Y < 3" )    
  dExt  <- createExternalParameters( 50, names = "E0,ED50,EMAX", 
    file = testParamFile, workingPath = parameters.datapath, dataId="ID",
    errStruc = "A", betNames = "B1,B2", betNums = "1,3", seed = 81, 
    )
  dAll  <- createParameters( 50, extNames = "E0,ED50,EMAX", 
    extFile = testParamFile, workingPath = parameters.datapath, 
    extErrStruc = "A", extBetween = "B1,B2", extBetweenNums = "1,3",  extDataId="ID",
    seed = 81, 
    genNames = "X,Y,Z", genFixedMean = "0,0,0", genFixedCov = "1", 
    genBetweenNames = "X,Y,Z", 
    genBetweenCov = "1", genBetweenMean = "0,0,0", genErrStruc = "A", 
    genRange = "Y < 3")
 
  checkTrue( identical( dNor[, c("SUBJ", "X", "Y", "Z") ] , dAll[, c("SUBJ", "X", "Y", "Z")] ) , 
    msg = "all + nor")   
  checkTrue( identical( dExt[,c("SUBJ", "E0", "ED50", "EMAX")], dAll[, c("SUBJ", "E0", "ED50", "EMAX")] ) , 
    msg = "all + ext")   
  checkEquals( dNor$PAROMIT , dAll$PAROMIT, 
    msg = "checking the PAROMIT")  
     
}

test.parameter.sf3 <- function() {

	checkException( createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "100,100,100", covariance = "1", betNames = "X,Y", 
      betCov = "1", betMean = "0,0", errStruc = "N", digits = -1), 
    msg = "digits should be positive")
  
  dataNone <- createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "100,100,100", covariance = "1", betNames = "X,Y", 
      betCov = "1", betMean = "0,0", errStruc = "N", digits = "2" )        
  dataAdd  <- createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "100,100,100", covariance = "1", betNames = "X,Y", 
      betCov = "1", betMean = "0,0", errStruc = "A", digits = "2")
  dataLogNorm <- createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "100,100,100", covariance = "1", betNames = "X,Y", 
      betCov = "1", betMean = "0,0", errStruc = "L", digits = "2")
  
  checkEquals( dataAdd[, c("X", "Y", "Z")], round(dataAdd[, c("X", "Y", "Z")],2), 
    msg  = "check the atomic digits (add)" )
  checkEquals( dataLogNorm[, c("X", "Y", "Z")], round(dataLogNorm[, c("X", "Y", "Z")],2), 
    msg  = "check the atomic digits (prop)" )
  checkEquals( dataNone[, c("X", "Y", "Z", "X.Between", "Y.Between")], 
    round(dataNone[, c("X", "Y", "Z", "X.Between", "Y.Between")],2), 
    msg  = "check the atomic digits (prop)" )
  

  dataNone <- createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "100,100,100", covariance = "1", betNames = "X,Y", 
      betCov = "1", betMean = "0,0", errStruc = "N", digits = "2,3,2" )        
  dataAdd  <- createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "100,100,100", covariance = "1", betNames = "X,Y", 
      betCov = "1", betMean = "0,0", errStruc = "A", digits = "2,3,2")
  dataLogNorm <- createNormalParameters( 50, names = "X,Y,Z" , 
      mean = "100,100,100", covariance = "1", betNames = "X,Y", 
      betCov = "1", betMean = "0,0", errStruc = "L", digits = "2,3,2")
  checkEquals( dataAdd$X, round(dataAdd$X,2),  msg  = "check the vector digits (add,X)" )
  checkEquals( dataAdd$Y, round(dataAdd$Y,3),  msg  = "check the vector digits (add,Y)" )
  checkEquals( dataAdd$Z, round(dataAdd$Z,2),  msg  = "check the vector digits (add,Y)" )

  checkEquals( dataLogNorm$X, round(dataLogNorm$X,2), msg  = "check the vector digits (Prop,X)" )
  checkEquals( dataLogNorm$Y, round(dataLogNorm$Y,3), msg  = "check the vector digits (Prop,Y)" )
  checkEquals( dataLogNorm$Z, round(dataLogNorm$Z,2), msg  = "check the vector digits (Prop,Y)" )
    
  checkEquals( dataNone$X,         round(dataNone$X,2),         msg  = "check the vector digits (None,X)" )
  checkEquals( dataNone$Y,         round(dataNone$Y,3),         msg  = "check the vector digits (None,Y)" )
  checkEquals( dataNone$Z,         round(dataNone$Z,2),         msg  = "check the vector digits (None,Y)" )
  checkEquals( dataNone$X.Between, round(dataNone$X.Between,2), msg  = "check the vector digits (None,X.Between)" )
  checkEquals( dataNone$Y.Between, round(dataNone$Y.Between,3), msg  = "check the vector digits (None,Y.Between)" )
   
}

# SF issue 8
# Tue Jul 24 10:40:43 BST 2007 @444 /Internet Time/
test.parameter.sf8 <- function(){
  
  # checking betCov = 0 by default
  dataNone <- createNormalParameters( 50, names = "X,Y,Z" ,
    mean = "0,50,100",  betNames = "X,Y", covariance = "1", 
    betMean = "0,0", errStruc = "N", seed = 99 )

  checkTrue( all( dataNone[,c("X.Between", "Y.Between")]  == 0), 
    msg = "checking 0 covariance by default for between effects")
    
  data.sf8 <- createNormalParameters( 500, names = "X,Y,Z" , mean = "0,50,100", seed = 99 )
  checkTrue(all(sweep( data.sf8[, c("X", "Y", "Z")] ,2, c(0,50,100), "-")==0) , 
    msg = "checking the 0 covariance by default")  
    
}

test.parameter.rangeExclude <- function(){
	
	# checking betCov = 0 by default
	checkException(createNormalParameters( 500, names = "X,Y,Z" , 
					mean = "0,50,100", covariance = "1", 
					betNames = "X,Y", betCov = "1", 
					errStruc = "A", seed = 99,
					range = "Y < 50", maxDraws = 1, parRangeTolerance = .9 ), msg = "Bad range specified")
	
	checkTrue(is.data.frame(createNormalParameters( 500, names = "X,Y,Z" , 
							mean = "0,50,100", covariance = "1", 
							betNames = "X,Y", betCov = "1", 
							errStruc = "A", seed = 99,
							range = "Y < 100", maxDraws = 1, parRangeTolerance = .9 )), msg = "Good range specified")
	
	warnData <- suppressWarnings(createNormalParameters( 500, names = "X,Y,Z" , 
					mean = "0,50,100", covariance = "1", 
					betNames = "X,Y", betCov = "1", 
					errStruc = "A", seed = 99,
					range = "Y < 50", parRangeTolerance = .1, maxDraws = 1 ))
	
	checkTrue(is.data.frame(warnData) & any(warnData$PAROMIT == 1), msg = "Dataset returned with parameter omits")
	
}
