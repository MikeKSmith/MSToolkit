
setEctdDataMethod("CSV")

systemTestPath <- system.file(package = "MSToolkit", "systemTest")
covariatesDataFile <- file.path( systemTestPath, "data", "testCovariates.csv")
parametersDataFile <- file.path( systemTestPath, "data", "testParam.csv")
resetEctdColNames()

test.generateData.call1 <- function() {

  ## set up a temp directory as the workingPath
  dir.create( workPath <- tempfile() )
  setEctdDataMethod("CSV")

  # Set up elements of the run
  respFun <- "E0 + (EMAX * D) / (ED50 + D)"
  seqMat <- cbind(c(0, 15, 30, 45), c(45, 15, 30, 0) ,c(45, 30, 15, 0))
  dFun <- function(data, prop) ifelse(data$HOUR <= 0, 0, sample(0:1, nrow(data), TRUE, c(1-prop, prop)))

  # Create a dummy file (to check if it is overwritten)
  dir.create( file.path(workPath, "ReplicateData"), showWarnings = FALSE)
  write.csv(data.frame(X=1:3, Y=1:3), file.path( workPath, "ReplicateData", "replicate0003.csv") )

  # Execute call 1
  genCall1 <- try(generateData(replicateN = 2, subjects = 500, treatPeriod = c(0, 1:3, 5), treatSeq = seqMat, treatProp = c(0.2, 0.2, 0.6),
	  conCovNames = c("ConCov1","ConCov2","ConCov3"), conCovMean = rep(0, 3), conCovVCov=diag(3), conCovCrit = "ConCov1 > 0",
	  disCovNames = "DisCov1,DisCov2,DisCov3", disCovVals=list(0:1,1:2,1:3), disCovProb = ".5,.5#.5,.5#.3,.3,.4",
	  genParNames = c("E0","ED50","EMAX"), genParMean = c(0, 50, 10), genParVCov=diag(c(5, 10, 0)), genParCrit = "E0 > 0",
	  genParBtwNames = c("E0", "EMAX"), genParBtwVCov = diag(2), genParErrStruc = "Additive",
	  respEqn = respFun, respCrit = c("MyResponse < 6","MyResponse > 0"), respVCov = 1, respName = "MyResponse",
    idCol="SUBJ", doseCol="D", timeCol="HOUR", trtCol="T", mcarProp=.05,
    dropFun = dFun, dropFunExtraArgs = list(prop = .05), interimSubj = c(.3, .6, .8, 1),
    missingFlag = "MISSFLAG", parOmitFlag = "PARFLAG", respOmitFlag = "RESPFLAG", interimCol = "INT", seed=1,
    workingPath = workPath, mcarRule = "HOUR > 0" ))
resetEctdColNames()

  # Check basics
  checkTrue(class(genCall1) != "try-error", msg = "Check the call was successful")
  checkTrue( file.exists( file.path(workPath, "ReplicateData") ), msg = "Check ReplicateData directory has been created")
  checkTrue( file.exists( file.path(workPath, "ReplicateData", "replicate0001.csv")), msg = "Check Replicate Data replicate0001.csv was created")
  checkTrue( file.exists( file.path(workPath, "ReplicateData", "replicate0002.csv")), msg = "Check Replicate Data replicate0002.csv was created" )
  checkTrue(!file.exists( file.path(workPath, "ReplicateData", "replicate0003.csv")), msg = "Check Replicate Data replicate0003.csv was removed")

  # Import the data
  x <- lapply(1:2, readData, dataType="Replicate", workingPath = workPath )

  # Check variables exist in the data
  checkTrue(all(c("SUBJ", "T", "HOUR", "D") %in% names(x[[1]])), msg = "Check dosing variables are in the data")
  checkTrue(all(c("ConCov1", "ConCov2", "ConCov3", "DisCov1", "DisCov2", "DisCov3") %in% names(x[[1]])), msg = "Check covariate variables are in the data")
  checkTrue(all(c("E0", "ED50", "EMAX", "PARFLAG") %in% names(x[[1]])), msg = "Check parameter variables are in the data")
  checkTrue(all(c("MyResponse", "RESPFLAG", "MISSFLAG", "INT") %in% names(x[[1]])), msg = "Check response variables are in the data")

  # Check dosing regimes set up
  theTreats <- unique(x[[1]][,c("T", "HOUR", "D")])
  theTreats <- theTreats[order(theTreats$T, theTreats$HOUR, theTreats$D),]
  checkTrue(nrow(theTreats) == 15 && all(theTreats$T == rep(1:3, each=5)), msg = "Check treatment column (T)")
  checkTrue(nrow(theTreats) == 15 && all(theTreats$HOUR == rep(c(0, 1:3, 5), 3)), msg = "Check time column (HOUR)")
  checkTrue(nrow(theTreats) == 15 && all(theTreats$D == as.vector(rbind(0, seqMat))), msg = "Check dose column (D)")

  # Check treatment allocation
  x1 <- unique(x[[1]][,c("SUBJ", "T")])
  x2 <- unique(x[[2]][,c("SUBJ", "T")])
  x2$SUBJ <- x2$SUBJ + 500
  treatAlloc <- rbind(x1, x2)
  checkTrue(nrow(treatAlloc) == 1000, msg = "Check all subjects returned and allocated")
  checkTrue(binom.test(table(treatAlloc$T != 1), p=.2)$p.value > 0.05, msg = "Check correct proportional allocation of treatment 1")
  #checkTrue(binom.test(table(treatAlloc$T != 2), p=.2)$p.value > 0.05, msg = "Check correct proportional allocation of treatment 2")
  checkTrue(binom.test(table(treatAlloc$T != 3), p=.6)$p.value > 0.05, msg = "Check correct proportional allocation of treatment 3")
  checkTrue(!all(x1$T == x2$T), msg = "Check continuous treatments differ between replicates")

  # Check continuous covariates
  conData1 <- x[[1]][!duplicated(x[[1]]$SUBJ),c("ConCov1", "ConCov2", "ConCov3")]
  conData2 <- x[[2]][!duplicated(x[[1]]$SUBJ),c("ConCov1", "ConCov2", "ConCov3")]
  checkTrue(!all(conData1$ConCov1 == conData2$ConCov1), msg = "Check covariates differ between replicates")
  checkTrue(all(conData1$ConCov1 >= 0), msg = "Check all of covariate 1 are greater than 0 (as specified in the criteria)")
  checkTrue(t.test(conData1$ConCov2)$p.value > 0.05, msg = "Check distribution of covariate 2")
  checkTrue(t.test(conData1$ConCov3)$p.value > 0.05, msg = "Check distribution of covariate 3")
  checkTrue(cor.test(conData1$ConCov2, conData1$ConCov3)$p.value > 0.05, msg = "Check for (lack of) correlation between covariates")

  # Check discrete covariates: disCovVals=list(0:1,1:2,1:3), disCovProb = ".5,.5#.5,.5#.3,.3,.4",
  disData1 <- x[[1]][!duplicated(x[[1]]$SUBJ),c("DisCov1", "DisCov2", "DisCov3")]
  disData2 <- x[[2]][!duplicated(x[[1]]$SUBJ),c("DisCov1", "DisCov2", "DisCov3")]
  checkTrue(!all(disData1$DisCov1 == disData2$DisCov1), msg = "Check discrete covariates differ between replicates")
  checkTrue(all(disData1$DisCov1 %in% 0:1), msg = "Check values for discrete covariate 1")
  checkTrue(all(disData1$DisCov2 %in% 1:2), msg = "Check values for discrete covariate 2")
  checkTrue(all(disData1$DisCov3 %in% 1:3), msg = "Check values for discrete covariate 3")
  checkTrue(all(binom.test(table(disData1$DisCov1 != 1), p=.5)$p.value >.05), msg = "Check proportion for discrete covariate 1")
  checkTrue(all(binom.test(table(disData1$DisCov2 != 1), p=.5)$p.value >.05), msg = "Check proportion for discrete covariate 1")
  checkTrue(all(binom.test(table(disData1$DisCov3 != 1), p=.3)$p.value >.05), msg = "Check proportion for discrete covariate 1 (value 1)")
  checkTrue(all(binom.test(table(disData1$DisCov3 != 3), p=.4)$p.value >.05), msg = "Check proportion for discrete covariate 1 (value 3)")

  # Check parameters
  parData <- x[[1]][,c("SUBJ", "E0", "ED50", "EMAX")]
  checkTrue(all(parData$ED50 == parData$ED50[1]), msg = "Check all ED50s are the same (no random effects)")
  checkTrue(!all(parData$E0 == parData$E0[1]), msg = "Check E0 effects differ between subject")
  checkTrue(!all(parData$EMAX == parData$EMAX[1]), msg = "Check EMAX effects differ between subject")
  checkTrue(all(tapply(parData$EMAX, parData$SUBJ, function(x) all(x == x[1]))), msg = "Check effects are the same within subject")
  parData <- parData[!duplicated(parData$SUBJ),]
  checkTrue(t.test(parData$EMAX, mu = 10)$p.value > 0.05, msg = "Check distribution of EMAX")

  # Check response
  respData <- x[[1]][,c("E0", "ED50", "EMAX", "D", "MyResponse", "RESPFLAG")]
  respData$PRED <- with(respData, E0 + (EMAX * D) / (ED50 + D))
  respData$RES <- respData$MyResponse - respData$PRED
  checkTrue(t.test(respData$RES[1:500])$p.value > .05, msg = "Check response variable has been created correctly")
  respTab <- table(respData$RESPFLAG, cut(respData$MyResponse, c(-100, 0, 6, 100)))
  checkTrue(nrow(respTab) == 2 & ncol(respTab) == 3 & sum(respTab) == nrow(respData), msg = "Check values have been generated outside the range")
  checkTrue(respTab[1,1] == 0 & respTab[2,2] == 0 & respTab[1,3] == 0, msg = "Check values outside response range have been flagged as omitted")

  # Check missings
  missTab <- table(x[[1]]$HOUR, x[[1]]$MISSFLAG)
  expectMCAR <- c(0, rep(.05*500, 4))
  expectDrop <- 0:4 * 0.05 * 475
  missTab <- cbind(missTab, MCAR = expectMCAR, DROP = expectDrop, EXPECT = expectMCAR + expectDrop )
  checkTrue(all(apply(missTab, 1, function(x) binom.test(x[2:1], p=x[5]/500)$p.value) > .05), msg = "Checking actual vs expected missing proportions")

  # Check interim data
  intData <- x[[1]][,c("SUBJ", "INT")]
  checkTrue(all(tapply(intData$INT, intData$SUBJ, function(x) all(x == x[1]))), msg = "Check interims allocated are the same within subject")
  intData <- intData[!duplicated(intData$SUBJ),]
  checkTrue(all(intData$INT %in% 1:4), msg = "Check values of interims allocated")
  checkTrue(binom.test(table(intData$INT != 1), p = .3)$p.value > 0.05, msg = "Check proportions of interims allocated (interim 1)")
  checkTrue(binom.test(table(intData$INT != 2), p = .3)$p.value > 0.05, msg = "Check proportions of interims allocated (interim 2)")
  checkTrue(binom.test(table(intData$INT != 3), p = .2)$p.value > 0.05, msg = "Check proportions of interims allocated (interim 3)")
  checkTrue(binom.test(table(intData$INT != 4), p = .2)$p.value > 0.05, msg = "Check proportions of interims allocated (interim 4)")

  unlink(workPath, recursive = TRUE)
  invisible(NULL)
}

test.generateData.call2 <- function() {

	## set up a temp directory as the workingPath
	dir.create( workPath <- tempfile() )
	setEctdDataMethod("CSV")

	# Set up elements of the run
	respFun <- "E0 + (EMAX * DOSE) / (ED50 + DOSE)"

	# Create a dummy file (to check if it is overwritten)
	dir.create( file.path(workPath, "ReplicateData") , showWarnings = FALSE)
	write.csv(data.frame(X=1:3, Y=1:3), file.path( workPath, "ReplicateData", "replicate0003.csv") )

	# Execute call 2
	resetEctdColNames()
	genCall2 <- try(generateData(replicateN = 2, subjects = 500, treatDoses = c(0, 15, 30), treatPeriod = 0:3,
					disCovNames = "DisCov1,DisCov2", disCovVals="1,2#1,2,3", disCovProbArray = rbind(c(.1, .1, .3), c(.3, .1, .1)),
					genParNames = c("E0","ED50","EMAX"), genParMean = c(0, 50, 10), genParVCov=diag(c(1, 0, 0)),
					genParBtwNames = c("E0", "EMAX"), genParBtwVCov = diag(2), genParErrStruc = "None",
					respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteCurrData = FALSE,
					workingPath = workPath))
	resetEctdColNames()

	# Check basics
	checkTrue(class(genCall2) != "try-error", msg = "Check the call was successful")
	checkTrue(file.exists( file.path( workPath, "ReplicateData")), msg = "Check ReplicateData directory has been created")
	checkTrue(!file.exists( file.path( workPath, "ReplicateData", "replicate0001.csv" ) ), msg = "Check Replicate Data replicate0001.csv was created")
	checkTrue(!file.exists( file.path( workPath, "ReplicateData", "replicate0002.csv" ) ), msg = "Check Replicate Data replicate0002.csv was created" )
	checkTrue(file.exists( file.path( workPath, "ReplicateData", "replicate0003.csv" ) ), msg = "Check (dummy) Replicate Data replicate0003.csv still exists" )
	checkTrue(file.exists( file.path( workPath, "ReplicateData", "replicate0004.csv" ) ), msg = "Check Replicate Data replicate0001.csv was created")
	checkTrue(file.exists( file.path( workPath, "ReplicateData", "replicate0005.csv" ) ), msg = "Check Replicate Data replicate0002.csv was created" )

	# Import the data
	x <- lapply(4:5, readData, dataType="Replicate", workingPath = workPath )

	# Check variables exist in the data
	checkTrue(all(c("SUBJ", "TIME", "DOSE", "TRT") %in% names(x[[1]])), msg = "Check dosing variables are in the data")
	checkTrue(all(c("DisCov1", "DisCov2") %in% names(x[[1]])), msg = "Check covariate variables are in the data")
	checkTrue(all(c("E0", "ED50", "EMAX", "E0.Extra", "EMAX.Extra", "RESP") %in% names(x[[1]])), msg = "Check parameter variables are in the data")

	# Check dosing regimes set up
	theTreats <- unique(x[[1]][,c("TRT", "TIME", "DOSE")])
	theTreats <- theTreats[order(theTreats$TRT, theTreats$TIME, theTreats$DOSE),]

	checkTrue(nrow(theTreats) == 12 && all(theTreats$TRT == rep(1:3, each=4)), msg = "Check treatment column (TRT)")
	checkTrue(nrow(theTreats) == 12 && all(theTreats$TIME == rep(0:3, 3)), msg = "Check time column (TIME)")
	expectDose <- c(rep(0, 4), rep(15, 4), rep(30, 4))
	checkTrue(nrow(theTreats) == 12 && all(theTreats$DOSE == expectDose), msg = "Check dose column (D)")

	# Check treatment allocation
	x1 <- unique(x[[1]][,c("SUBJ", "TRT")])
	x2 <- unique(x[[2]][,c("SUBJ", "TRT")])
	checkTrue(all(x1$T == x2$T), msg = "Check continuous treatments differ between replicates")

	# Check discrete covariates
	disData1 <- x[[1]][!duplicated(x[[1]]$SUBJ),c("DisCov1", "DisCov2")]
	disData2 <- x[[2]][!duplicated(x[[1]]$SUBJ),c("DisCov1", "DisCov2")]
	checkTrue(all(disData1$DisCov1 == disData2$DisCov1), msg = "Check discrete covariates are the same between replicates")
	checkTrue(all(disData1$DisCov1 %in% 1:2), msg = "Check values for discrete covariate 1")
	checkTrue(all(disData1$DisCov2 %in% 1:3), msg = "Check values for discrete covariate 2")
	checkTrue(binom.test(table(disData1$DisCov1 != 1), p = .5)$p.value > .05, msg = "Check overall proportion for covariate 1")
	checkTrue(binom.test(table(disData1$DisCov2 != 1), p = .4)$p.value > .05, msg = "Check overall proportion for covariate 2")
	disTest1 <- disData1$DisCov1 == 1 & disData1$DisCov2 == 1
	disTest2 <- disData1$DisCov1 == 2 & disData1$DisCov2 == 1
	checkTrue(binom.test(table(!disTest1), p = .1)$p.value > .05, msg = "Check multinomial proportion (1,1)")
	checkTrue(binom.test(table(!disTest2), p = .3)$p.value > .05, msg = "Check multinomial proportion (2,1)")

	# Check parameters
	parData <- x[[1]][,c("SUBJ", "E0", "ED50", "EMAX", "E0.Extra", "EMAX.Extra")]
	checkTrue(all(parData$ED50 == 50), msg = "Check all ED50s are the same")
	checkTrue(all(parData$E0 == parData$E0[1]), msg = "Check all E0s are the same")
	checkTrue(all(parData$EMAX == 10), msg = "Check all EMAXs are the same")
	checkTrue(!all(parData$E0.Extra == parData$E0.Extra[1]), msg = "Check all E0.Extras are different")
	checkTrue(!all(parData$EMAX.Extra == parData$EMAX.Extra[1]), msg = "Check all EMAX.Extras are different")
	checkTrue(abs(parData$E0[1]) < 4, msg = "Check E0 is in expected range")
	checkTrue(all(tapply(parData$EMAX.Extra, parData$SUBJ, function(x) all(x == x[1]))), msg = "Check effects are the same within subject")
	parData <- parData[!duplicated(parData$SUBJ), c("E0.Extra", "EMAX.Extra")]
	checkTrue(t.test(parData$EMAX.Extra)$p.value > 0.05, msg = "Check distribution of EMAX.Extra")

	# Check response
	respData <- x[[1]][,c("E0", "ED50", "EMAX", "DOSE", "RESP")]
	respData$PRED <- with(respData, E0 + (EMAX * DOSE) / (ED50 + DOSE))
	respData$RES <- respData$RESP - respData$PRED
	checkTrue(all(round(respData$RES, 3) == 0), msg = "Check response variable has been created correctly")

	unlink(workPath, recursive = TRUE)
	invisible(NULL)
}

test.generateData.call3 <- function() {

  dir.create( td3 <- tempfile() )
  file.copy(covariatesDataFile, td3 )
  file.copy(parametersDataFile, td3 )

  # Set up elements of the run
  respFun <- "X1 + X2 + X3"

  # Execute call 3
  genCall3 <- try(generateData(replicateN = 1, subjects = 10, treatDoses = 1,
    extCovNames = "X1,X2,X3", extCovFile = "testCovariates.csv", extCovSubset = "X2 > 0",
    extCovSameRow = TRUE, extCovDataId = "ID", extCovRefCol = "ID",
    extParNames = "E0,ED50,EMAX", extParBtwNames = "B1,B2", extParBtwNums = c(1, 3),
    extParErrStruc = "None", extParDataId = "ID",
    extParFile = "testParam.csv", extParSubset = "B1 > 0",
    respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1,
    parBtwSuffix=".Extra", deleteCurrData = TRUE, workingPath = td3))
  x3 <- readData(1, dataType="Replicate", workingPath = td3)
  resetEctdColNames()

  # Execute call 4
  genCall4 <- try(generateData(replicateN = 1, subjects = 10, treatDoses = 1,
    extCovNames = "X1,X2,X3", extCovFile = "testCovariates.csv", extCovSubset = "X2 > 0", extCovSameRow = FALSE,
    extCovDataId = "ID",
    extParNames = "E0,ED50,EMAX", extParBtwNames = "B1,B2", extParBtwNums = c(1, 3), extParErrStruc = "Additive",
    extParFile = "testParam.csv", extParSubset = "B1 > 0",  extParDataId = "ID",
    respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteCurrData = TRUE, workingPath = td3))
  x4 <- readData(1, dataType="Replicate", workingPath = td3)
  resetEctdColNames()

  # Execute call 5
  genCall5 <- try(generateData(replicateN = 1, subjects = 10, treatDoses = 1,
    extCovNames = "X1,X2,X3", extCovFile = "testCovariates.csv", extCovSubset = "X2 > 0", extCovSameRow = TRUE, extCovDataId = "ID", extCovRefCol = "ID",
    extParNames = "E0,ED50,EMAX", extParBtwNames = "B1,B2", extParBtwNums = c(1, 3), extParErrStruc = "None",
    extParFile = "testParam.csv", extParRefColData = "ID", extParRefColName = "ID", extParDataId = "ID",
    respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteCurrData = TRUE, workingPath = td3))
  x5 <- readData(1, dataType="Replicate", workingPath = td3)
  resetEctdColNames()

  # Check basics
  checkTrue(class(genCall3) != "try-error", msg = "Check call 3 was successful")
  checkTrue(class(genCall4) != "try-error", msg = "Check call 4 was successful")

  # Check covariates
  checkTrue(all(x3$X3 %in% 1:10), msg = "Check imported covariate values")
  checkTrue(all(x3$X1 == x3$X2), msg = "Check all covariates taken from same line")
  checkTrue(!all(x4$X1 == x4$X2), msg = "Check all covariates not taken from same line")
  checkTrue(all(x3$X1 == x3$ID.refCol), msg = "Check reference column correctly created")
  checkTrue(all(x4$X2 >= 0), msg = "Check covariate subset works")

  # Check Parameters
  checkTrue(all(c("B1", "B2") %in% names(x3)), msg = "Check between subject variables imported correctly")
  checkTrue(!any(c("B1", "B2") %in% names(x4)), msg = "Check between subject variables removed correctly")
  checkTrue(all(round(x3$E0 + x3$B1 - x4$E0, 3) == 0), msg = "Check random effects correctly applied")
  checkTrue(all(x3$B1 >= 0), msg = "Check between subject subset applied correctly")

  # Check refCol in parameters
  importPars <- read.csv(parametersDataFile, sep=",", header=TRUE)
  importPars <- importPars[!duplicated(importPars$ID), c("ID", "B1", "B2")]
  names(importPars) <- c("ID", "Orig1", "Orig2")
  importPars <- merge(importPars, x5[,c("ID.refCol", "B1", "B2")], by.x="ID", by.y="ID.refCol")
  checkTrue(all(round(importPars$B1 - importPars$Orig1, 3) == 0), msg = "Check reference column functionality")

  try( unlink(td3, recursive = TRUE) )
  invisible(NULL)
}

### change request 29aug07
### Wed Aug 29 11:04:59 BST 2007 @461 /Internet Time/
# testing that the default covariance works for generateData call
test.changeRequest19aug07 <- function( ){

  ## set up a temp directory as the workingPath
  dir.create( workPath <- tempfile() )

  # Set up elements of the run
  respFun <- "E0 + (EMAX * DOSE) / (ED50 + DOSE)"

  # Create a dummy file (to check if it is overwritten)
  dir.create( file.path(workPath, "ReplicateData") , showWarnings = FALSE)

  # Execute call 2
  genCall1 <- try(generateData(replicateN = 2, subjects = 500, treatDoses = c(0, 15, 30), treatPeriod = 0:3,
    disCovNames = "DisCov1,DisCov2", disCovVals="1,2#1,2,3", disCovProbArray = rbind(c(.1, .1, .3), c(.3, .1, .1)),
	  genParNames = c("E0","ED50","EMAX"), genParMean = c(0, 50, 100) , genParErrStruc = "None",
	  respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteCurrData = FALSE,
    workingPath = workPath))
	resetEctdColNames()

  checkTrue(class(genCall1) != "try-error", msg = "Check call was successful")

  x <- do.call( rbind, lapply(1:2, readData, dataType="Replicate", workingPath = workPath ) )

  checkTrue( all( x[, "E0"] == 0), msg = "check default vcov matrix to 0 ()" )
  checkTrue( all( x[, "ED50"] == 50),msg = "check default vcov matrix to 0 ()" )
  checkTrue( all( x[, "EMAX"] == 100),msg = "check default vcov matrix to 0 ()" )

}

test.generateData.testDelimiters <- function() {

	# Set up path and files
	dir.create( testDelims <- tempfile() )
	otherFiles <- c("testParam.nonmem", "testParam.tabDelim", "testParam.spaceDelim")

	# Try a high level call for each file type
	for (i in otherFiles) {
		file.copy( file.path( systemTestPath, "data", i), testDelims )
		genCall3 <- try(generateData(replicateN = 1, subjects = 10, treatDoses = 1,
						extParNames = "E0,ED50,EMAX", extParBtwNames = "B1,B2", extParBtwNums = c(1, 3),
						extParErrStruc = "None", extParDataId = "ID",
						extParFile = i, extParSubset = "B1 > 0",
						respEqn = "E0 + ED50 + EMAX", treatDiff = FALSE, seed = 1,
						parBtwSuffix=".Extra", deleteCurrData = FALSE, workingPath = testDelims))

		x <- readData(1, dataType="Replicate", workingPath = testDelims)
		resetEctdColNames()

		checkTrue(all(x$RESP == 148.722), msg = paste("Check parameter data correctly imported from", i))
	}
	try( unlink(testDelims, recursive = TRUE) )
	invisible(NULL)
}

test.generateData.Version2.0 <- function() {

	# Reset column names + look at current data method
	resetEctdColNames()
	nowMethod <- getEctdDataMethod()
	setEctdDataMethod("CSV")

	# Allow generation of simulated data from a single function call
	whichDir <- tempdir()
	suppressWarnings(dir.create(whichDir))
	thePath <- file.path(whichDir, "genTest")
	suppressWarnings(dir.create(thePath))
	generateData(1, 5, treatDoses = 0, respEqn = "SUBJ", workingPath = thePath)
	genFiles <- list.files(file.path(thePath, "ReplicateData"))
	checkTrue(length(genFiles) == 1 && genFiles == "replicate0001.csv")
	getData <- readAllData(workingPath = thePath)
	checkTrue( nrow(getData) == 5 )
	checkTrue( all(getData$Replicate == 1) & all(getData$SUBJ == getData$RESP) & all(getData$SUBJ == 1:5) )
	try(unlink(thePath, recursive = TRUE))

	# Allow setting of random number seed for reproducibility
	whichDir <- tempdir()
	suppressWarnings(dir.create(whichDir))
	thePath <- file.path(whichDir, "genTest")
	suppressWarnings(dir.create(thePath))
	generateData(2, 15, treatDoses = c(0, 15), treatPeriod = 1:2,
		respEqn = "SUBJ + A + B + a + b + c",
		workingPath = thePath, seed = 123, genParNames = letters[1:3],
		genParMean = 1:3, genParVCov = 1, genParBtwNames = "b",
		genParBtwVCov = 1, conCovNames = LETTERS[1:2], conCovMean = 1:2,
		conCovVCov = 1, respVCov = 1, genParErrStruc = "Additive")
	getData1 <- readAllData(workingPath = thePath)
	generateData(2, 15, treatDoses = c(0, 15), treatPeriod = 1:2,
		respEqn = "SUBJ + A + B + a + b + c",
		workingPath = thePath, seed = 123, genParNames = letters[1:3],
		genParMean = 1:3, genParVCov = 1, genParBtwNames = "b",
		genParBtwVCov = 1, conCovNames = LETTERS[1:2], conCovMean = 1:2,
		conCovVCov = 1, respVCov = 1, genParErrStruc = "Additive")
	getData2 <- readAllData(workingPath = thePath)
	try(unlink(thePath, recursive = TRUE))
	checkTrue(identical(getData1, getData2))

	# Allow setting of key column names, such as "Subject", "Time" and "Dose"
	whichDir <- tempdir()
	suppressWarnings(dir.create(whichDir))
	thePath <- file.path(whichDir, "genTest")
	suppressWarnings(dir.create(thePath))
	generateData(1, 10, treatDoses = c(0, 15), treatPeriod = 1:2,
			respEqn = ".id + A + B + a + b + c",
			workingPath = thePath, seed = 123, genParNames = letters[1:3],
			genParMean = 1:3, genParVCov = 1, genParBtwNames = "b",
			genParBtwVCov = 1, conCovNames = LETTERS[1:2], conCovMean = 1:2,
			conCovVCov = 1, respVCov = 1, genParErrStruc = "Additive",
			interimSubj = c(.3, .7, 1), mcarProp = .1, idCol = ".id",
			doseCol = ".dose", timeCol = ".time", trtCol = ".trt",
			parOmitFlag = ".paromit", missingFlag = ".missing", respOmitFlag = ".respomit",
			interimCol = ".interim", respName = ".response")
	getData1 <- readAllData(workingPath = thePath, replicateCol = ".replicate")
	checkException(generateData(1, 10, treatDoses = c(0, 15), treatPeriod = 1:2,
					respEqn = ".id + A + B + a + b + c",
					workingPath = thePath, seed = 123, genParNames = letters[1:3],
					genParMean = 1:3, genParVCov = 1, genParBtwNames = "b",
					genParBtwVCov = 1, conCovNames = LETTERS[1:2], conCovMean = 1:2,
					conCovVCov = 1, respVCov = 1, genParErrStruc = "Additive",
					interimSubj = c(.3, .7, 1), mcarProp = .1))
	generateData(1, 2, treatDoses = 0, treatPeriod = 1:2,
			respEqn = "SUBJ + A + B + a + b + c",
			workingPath = thePath, seed = 123, genParNames = letters[1:3],
			genParMean = 1:3, genParVCov = 1, genParBtwNames = "b",
			genParBtwVCov = 1, conCovNames = LETTERS[1:2], conCovMean = 1:2,
			conCovVCov = 1, respVCov = 1, genParErrStruc = "Additive",
			interimSubj = c(.3, .7, 1), mcarProp = .1)
	getData2 <- readAllData(workingPath = thePath)
	try(unlink(thePath, recursive = TRUE))
	resetEctdColNames()
	expectNames <- c(".replicate", ".dose", ".trt", ".id", ".time", ".paromit", ".respomit", ".response", ".missing", ".interim")
	checkTrue( all(expectNames %in% names(getData1)) )
	checkTrue( !any(expectNames %in% names(getData2)) )

	# Allow control over whether to overwrite previous data or append to it
	# For completeness, we will test for each data method

	# Setup
	whichDir <- tempdir()
	suppressWarnings(dir.create(whichDir))
	thePath <- file.path(whichDir, "genTest")
	suppressWarnings(dir.create(thePath))

	# CSV method
	setEctdDataMethod("CSV")
	generateData(2, 5, treatDoses = 0, respEqn = "SUBJ", workingPath = thePath)
	genFiles <- list.files(file.path(thePath, "ReplicateData"))
	checkTrue( length(genFiles) == 2 )
	generateData(2, 5, treatDoses = 0, respEqn = "SUBJ", workingPath = thePath, deleteCurrData = FALSE)
	genFiles <- list.files(file.path(thePath, "ReplicateData"))
	checkTrue(  length(genFiles) == 4 )
	generateData(2, 5, treatDoses = 0, respEqn = "SUBJ", workingPath = thePath)
	genFiles <- list.files(file.path(thePath, "ReplicateData"))
	checkTrue ( length(genFiles) == 2 )

	# RData method
	setEctdDataMethod("RData")
	generateData(2, 5, treatDoses = 0, respEqn = "SUBJ", workingPath = thePath)
	genFiles <- list.files(file.path(thePath, "ReplicateData"))
	checkTrue(length(genFiles) == 2)
	generateData(2, 5, treatDoses = 0, respEqn = "SUBJ", workingPath = thePath, deleteCurrData = FALSE)
	genFiles <- list.files(file.path(thePath, "ReplicateData"))
	checkTrue(length(genFiles) == 4)
	generateData(2, 5, treatDoses = 0, respEqn = "SUBJ", workingPath = thePath)
	genFiles <- list.files(file.path(thePath, "ReplicateData"))
	checkTrue(length(genFiles) == 2)

	# Remove directories
	try(unlink(thePath, recursive = TRUE))

	# Internal method
	setEctdDataMethod("Internal")
	generateData(2, 5, treatDoses = 0, respEqn = "SUBJ")
	checkTrue(length(.ectdEnv$DataStore) == 2)
	generateData(2, 5, treatDoses = 0, respEqn = "SUBJ", deleteCurrData = FALSE)
	checkTrue(length(.ectdEnv$DataStore) == 4)
	generateData(2, 5, treatDoses = 0, respEqn = "SUBJ")
	checkTrue(length(.ectdEnv$DataStore) == 2)

	# Empty Internal Data Store
	.ectdEnv$DataStore <- NULL
	resetEctdColNames()

	# Allow creation of continous covariates to be included in the dataset

	# Setup: Use internal method
	nowMethod <- getEctdDataMethod()
	setEctdDataMethod("Internal")

	# Generate data
	generateData(2, 5, treatDoses = 0, respEqn = "SUBJ",
			conCovNames = LETTERS[1:3], conCovMean = 1:3,
			conCovVCov = diag(1:3), conCovCrit = "2 <= C <= 4",
			conCovDigits = 1:3, covDiff = FALSE)
	getData1 <- readAllData()
	generateData(2, 1000, treatDoses = 0, respEqn = "SUBJ",
			conCovNames = LETTERS[1:3], conCovMean = 5:7,
			conCovVCov = diag(c(1,2,1)),  conCovCrit = "5 <= C <= 9",)
	getData2 <- readAllData()

	# Tests: 1st data example should have same covariates in each replicate
	splitData1 <- split(getData1[LETTERS[1:3]], getData1$Replicate)
	splitData1 <- lapply(splitData1, function(df) {
			row.names(df) <- as.character(1:nrow(df))
			df
	})

	checkTrue(identical(splitData1[[1]], splitData1[[2]]))

	# Tests: 2nd data example should not have same covariates in each replicate
	splitData2 <- split(getData2[LETTERS[1:3]], getData2$Replicate)
	splitData2 <- lapply(splitData2, function(df) {
				row.names(df) <- as.character(1:nrow(df))
				df
			})
	checkTrue(!identical(splitData2[[1]], splitData2[[2]]))

	# Tests: Check distribution of generated covariates
	getData <- getData2[LETTERS[1:3]]
	statTest1 <- t.test(getData$A, mu = 5)$p.value > .025
	statTest2 <- t.test(getData$B, mu = 6)$p.value > .025
	statTest3 <- t.test(getData$C, mu = 7)$p.value > .025
	checkTrue(all(statTest1, statTest2, statTest3))
	checkTrue(all(getData$C >= 5 & getData$C <= 9))
	checkTrue(var(getData$B) > var(getData$A))

	# Finish up
	.ectdEnv$DataStore <- NULL
	resetEctdColNames()

	# Allow creation of discrete covariates to be included in the dataset

	# Setup: Use internal method
	nowMethod <- getEctdDataMethod()
	setEctdDataMethod("Internal")

	# Test 1: explicit call
	N <- 100
	generateData(1, N, treatDoses = 0, respEqn = "SUBJ",
			disCovNames = LETTERS[1:3], disCovVals = list(0:1, 1:4, c(0, 15, 30)),
			disCovProb = list(rep(.5, 2), c(.5, .1, .2, .2), c(0, .5, .5)))
	getData1 <- readAllData()[LETTERS[1:3]]
	aTable <- as.vector(table(getData1$A))
	bTable <- as.vector(table(getData1$B))
	cTable <- as.vector(table(getData1$C))

	checkTrue(all(getData1$A %in% 0:1) & all(getData1$B %in% 1:4) & all(getData1$C %in% c(15, 30)))
	checkTrue(prop.test(aTable[1], N, .5)$p.value > .01)
	checkTrue(prop.test(bTable[1], N, .5)$p.value > .01)
	checkTrue(prop.test(cTable[2], N, .5)$p.value > .01)

	# Test2: Using "disCovProbArray"
	padf <- data.frame(
			F1 = rep(0:1, 3),
			F2 = rep(1:3, each = 2),
			PROB = c(.1,.2,.1,.2,.2,.2))
	generateData(1, N, treatDoses = 0, respEqn = "SUBJ", disCovProbArray = padf)
	getData2 <- readAllData()[c("F1", "F2")]
	aggData <- aggregate(list(N = getData2$F1), getData2[c("F1", "F2")], length)
	mergeData <- merge(padf, aggData)
	pVals <- c()
	for (i in 1:nrow(mergeData)) pVals[i] <- prop.test(mergeData$N[i], N, mergeData$PROB[i])$p.value > .025
	checkTrue(sum(pVals) >= 5)

	# Finish up
	.ectdEnv$DataStore <- NULL
	resetEctdColNames()

	# Allow definition of treatments for subjects
	setEctdDataMethod("Internal")

	# Testing parallel, order and treatSubj
	generateData(1, 20, treatSubj = c(10, 5, 5), treatDoses = 1:3, respEqn = "SUBJ", treatOrder = FALSE, seed = 123)
	generateData(1, 20, treatSubj = c(10, 5, 5), treatDoses = 1:3, respEqn = "SUBJ", treatOrder = TRUE, deleteCurrData = FALSE, seed = 123)
	getData <- readAllData()
	splitDose <- split(getData$DOSE, getData$Replicate)
	checkTrue(all(1:3 %in% splitDose[[1]]))
	checkTrue(!all(splitDose[[1]] == splitDose[[2]]))
	checkTrue(all(sort(splitDose[[1]]) == splitDose[[2]]))
	checkTrue(all(as.vector(table(splitDose[[2]])) == c(10, 5, 5)))

	# Testing parallel, proportion & treatDiff
	N <- 50
	generateData(2, N, treatPeriod = 1:3, treatDoses = 1:3,
			treatProp = c(.5, .25, .25), respEqn = "SUBJ",
			seed = 111, treatDiff = TRUE)
	generateData(2, N, treatPeriod = 1:3, treatDoses = 1:3,
			treatProp = c(.5, .25, .25), respEqn = "SUBJ",
			seed = 111, treatDiff = FALSE, deleteCurrData = FALSE)
	getData <- readAllData()
	uniData <- unique(getData[c("Replicate", "SUBJ", "DOSE")])
	checkTrue(nrow(uniData) == N * 4)  # Should be parallel - only 1 dose per ID
	splitDose <- split(uniData$DOSE, uniData$Replicate)
	checkTrue(!all(splitDose[[1]] == splitDose[[2]]))
	checkTrue(all(splitDose[[3]] == splitDose[[4]]))
	allDoses <- getData$DOSE [ getData$Replicate %in% 1:2 & getData$TIME == 1 ]
	doseTable <- as.vector(table(allDoses))
	checkTrue(prop.test(doseTable, rep(N*2, 3), c(.5, .25, .25))$p.value > .005)

	# Testing crossover design and run-in
	N <- 20
	generateData(1, N, treatPeriod = -1:3, treatDoses = 1:3,
			respEqn = "SUBJ", treatDiff = TRUE)
	getData <- readAllData()
	checkTrue(all(getData$DOSE [ getData$TIME < 0 ] == 0))
	checkTrue(!any(getData$DOSE [ getData$TIME >= 0 ] == 0))
	generateData(1, N, treatSeq = cbind(c(0, 15, 30), c(15, 30, 0) ,c(30, 15, 0)),
			treatPeriod = -1:2, respEqn = "SUBJ", treatDiff = TRUE)
	getData <- readAllData()
	checkTrue(all(getData$DOSE [ getData$TIME < 0 ] == 0))
	getData <- getData [ getData$TIME >= 0, ]
	allDoses <- sapply(split(getData$DOSE, getData$SUBJ), paste, collapse=", ")
	checkTrue(all(allDoses %in% c("0, 15, 30", "15, 30, 0", "30, 15, 0")))

	# Finish
	.ectdEnv$DataStore <- NULL
	resetEctdColNames()

	# Allow allocation of treatments to generated subjects
	setEctdDataMethod("Internal")

	# Testing parallel, proportion & treatDiff
	N <- 250
	generateData(1, N, treatDoses = 1:3, treatProp = c(.5, .25, .25), respEqn = "SUBJ")
	generateData(1, N, treatDoses = 1:3, respEqn = "SUBJ", deleteCurrData = FALSE)
	getData <- readAllData()[c("Replicate", "DOSE")]
	splitDose <- as.vector(unlist(lapply(split(getData$DOSE, getData$Replicate), function(x) as.vector(table(x)))))
	probVec <- c(.5, .25, .25, .33, .33, .33)
	checkTrue(prop.test(splitDose, rep(N, 6), probVec)$p.value > .01)

	# Finish
	.ectdEnv$DataStore <- NULL
	resetEctdColNames()

	# Allow creation of fixed and between subject model
	# parameters based on a multivariate normal distribution

	setEctdDataMethod("Internal")

	# Basic test
	N <- 50
	generateData(N, 5, treatDoses = 0, respEqn = "A + B + C",
			genParNames = LETTERS[1:3], genParMean = 1:3, genParVCov = 1)
	getData <- unique(readAllData()[c("Replicate", LETTERS[1:3])])
	checkTrue(nrow(getData) == N)
	checkTrue(t.test(getData$A, mu = 1)$p.value > .01)
	checkTrue(t.test(getData$B, mu = 2)$p.value > .01)
	checkTrue(t.test(getData$C, mu = 3)$p.value > .01)

	# Between subject error
	N <- 5
	generateData(N, 5, treatDoses = 0, treatPeriod = 1:3,
			respEqn = "A + B + C", genParNames = LETTERS[1:3], genParMean = 1:3,
			genParVCov = 1, genParBtwNames = c("A", "C"), genParBtwMean = 10,
			genParBtwVCov = 1, genParErrStruc = "Additive", seed = 123)
	getData1 <- readAllData()
	generateData(N, 5, treatDoses = 0, treatPeriod = 1:3,
			respEqn = "A + B + C", genParNames = LETTERS[1:3], genParMean = 1:3,
			genParVCov = 1, genParBtwNames = c("A", "C"), genParBtwMean = 10,
			genParBtwVCov = 1, genParErrStruc = "None", seed = 123)
	getData2 <- readAllData()
	errDiff <- round(getData2$A + getData2$A.Between - getData1$A, 2)
	checkTrue(nrow(getData1) == N * 5 * 3 & nrow(getData2) == N * 5 * 3)
	checkTrue(nrow(unique(getData1[c("Replicate", "B")])) == 5)
	checkTrue(nrow(unique(getData1[c("Replicate", "A")])) == N * 5)
	checkTrue(nrow(unique(getData1[c("Replicate", "C")])) == N * 5)
	checkTrue(nrow(unique(getData2[c("Replicate", "A", "B", "C")])) == 5)
	btwNames <- paste(c("A", "C"), "Between", sep=".")
	checkTrue(!any(btwNames %in% names(getData1)) & all(btwNames %in% names(getData2)))
	checkTrue(all(errDiff == 0))
	getData2 <- getData2 [ !duplicated(getData2[c("Replicate", "SUBJ")]), ]
	btwVals <- c(getData2$A.Between, getData2$C.Between)
	checkTrue(t.test( btwVals, mu = 10)$p.value > .025)

	# Finish
	.ectdEnv$DataStore <- NULL
	resetEctdColNames()

	# Allow creation of reponse variable based
	# on R function or character string

	# Setup
	setEctdDataMethod("Internal")

	# Basic test
	generateData(1, 5, treatDoses = 15, respEqn = "DOSE + SUBJ + 1")
	getData <- readAllData()
	checkTrue(with(getData, all(RESP == DOSE + SUBJ + 1)))
	respFun <- function(data) with(data, DOSE + SUBJ + 1)
	generateData(1, 5, treatDoses = 15, respEqn = respFun)
	getData <- readAllData()
	checkTrue(with(getData, all(RESP == DOSE + SUBJ + 1)))

	# Finish
	.ectdEnv$DataStore <- NULL

	# Allow generation and application of
	# residual error to the generated response

	# Setup
	setEctdDataMethod("Internal")

	# Basic test
	N <- sample(1:9999, 1)
	generateData(1, 5, treatDoses = 15, respEqn = "SUBJ", respVCov = 1, respErrStruc = function(x, y) y, seed = N, respDigits = 6)
	getData1 <- readAllData()
	generateData(1, 5, treatDoses = 15, respEqn = "SUBJ", respVCov = 1, respErrStruc = "A", seed = N, respDigits = 6)
	getData2 <- readAllData()
	generateData(1, 5, treatDoses = 15, respEqn = "SUBJ", respVCov = 1, respErrStruc = "P", seed = N, respDigits = 6)
	getData3 <- readAllData()
	generateData(1, 5, treatDoses = 15, respEqn = "SUBJ", respVCov = 1, respErrStruc = "L", seed = N, respDigits = 6)
	getData4 <- readAllData()
	allData <- data.frame(ID = getData1$SUBJ, ERR = getData1$RESP, ADD = getData2$RESP, PROP = getData3$RESP, LOGN = getData4$RESP)
	checkTrue(all(with(allData, round(ADD, 3) == round(ID + ERR, 3))))
	checkTrue(all(with(allData, round(PROP, 2) == round(ID * (1 + ERR), 2))))
	checkTrue(all(with(allData, round(LOGN, 2) == round(ID * exp(ERR), 2))))

	# Finish
	.ectdEnv$DataStore <- NULL
	resetEctdColNames()

	# Allow creation of "missing at random" data
	nowMethod <- getEctdDataMethod()
	setEctdDataMethod("Internal")

	# Basic test
	.ectdEnv$DataStore <- NULL
	generateData(1, 100, mcarProp = 0.1, treatPeriod = 1:3, treatDoses = 15, respEqn = "SUBJ", deleteCurrData = FALSE)
	generateData(1, 100, mcarProp = 0.5, treatPeriod = 1:3, treatDoses = 15, respEqn = "SUBJ", deleteCurrData = FALSE)
	generateData(1, 100, mcarProp = 0.5, mcarRule = "TIME > 1", treatPeriod = 1:3, treatDoses = 15, respEqn = "SUBJ", deleteCurrData = FALSE)
	getData <- readAllData()
	checkTrue(with(getData, !any(TIME [ Replicate == 3 & MISSING == 1] < 2)))
	p10 <- sum(getData$MISSING [ getData$Replicate == 1 ])
	p50 <- sum(getData$MISSING [ getData$Replicate == 2 ]) + sum(getData$MISSING [ getData$Replicate == 3 & getData$TIME > 1 ])
	n10 <- sum(getData$Replicate == 1)
	n50 <- sum(getData$Replicate == 2) + sum(getData$Replicate == 3 & getData$TIME > 1)
	checkTrue(prop.test(c(p10, p50), c(n10, n50), c(.1, .5))$p.value > .01)

	# Finish
	.ectdEnv$DataStore <- NULL
	resetEctdColNames()

	# Allow creation of "dropout" data
	setEctdDataMethod("Internal")

	# Basic test
	dFun1 <- function(data) as.numeric(data$TIME > 2)
	dFun2 <- function(data) data$TIME > 1 & sample(0:1, nrow(data), T)
	generateData(1, 100, dropFun = dFun1, treatPeriod = 1:3, treatDoses = 15, respEqn = "SUBJ", seed = 123, deleteCurrData = TRUE)
	generateData(1, 100, dropFun = dFun2, treatPeriod = 1:3, treatDoses = 15, respEqn = "SUBJ", seed = 123, deleteCurrData = FALSE)
	getData <- readAllData()[c("Replicate", "SUBJ", "TIME", "MISSING")]
	R1 <- subset(getData , Replicate == 1)
	R2 <- subset(getData , Replicate == 2)
	checkTrue(all(R1$MISSING [ R1$TIME > 2] == 1))
	checkTrue(all(R1$MISSING [ R1$TIME <= 2] == 0))
	checkTrue(all(R2$MISSING [ R2$TIME <= 1] == 0))
	checkDirection <- tapply(R2$MISSING, R2$SUBJ, function(vec) all(diff(vec) >= 0))
	checkTrue(all(checkDirection))

	# Finish
	.ectdEnv$DataStore <- NULL
	resetEctdColNames()

	# Allow allocation of subjects to "interims"
	setEctdDataMethod("Internal")

	# Basic test
	generateData(1, 100, interimSubj = c(.5, 1), treatPeriod = 1:3, treatDoses = 15, respEqn = "SUBJ", seed = 123, deleteCurrData = TRUE)
	generateData(1, 100, interimSubj = c(.2, .7, 1), treatPeriod = 1:3, treatDoses = 15, respEqn = "SUBJ", seed = 123, deleteCurrData = FALSE)
	getData <- readAllData()

	# Tests
	uniData <- unique(getData[c("Replicate", "SUBJ", "INTERIM")])
	checkTrue(nrow(uniData) == 2 * 100)
	aggData <- aggregate(list(N = uniData$Replicate), uniData[c("INTERIM", "Replicate")], length)
	sumData <- aggregate(list(TOTAL = uniData$Replicate), uniData["Replicate"], length)
	mData <- merge(aggData, sumData)

	probData <- data.frame( Replicate = c(1, 1, 2, 2, 2), INTERIM = c(1:2, 1:3), PROB = c(.5, .5, .2, .5, .3))
	mData <- merge(mData, probData)
	checkTrue(prop.test(mData$N, mData$TOTAL, mData$PROB)$p.value > .01)

	# Finish
	.ectdEnv$DataStore <- NULL
	resetEctdColNames()

	# Allow writing of generated data to "CSV" files, "RData"
	# files or an "internal" storage location

	# Setup
	whichDir <- tempdir()
	suppressWarnings(dir.create(whichDir))
	thePath <- file.path(whichDir, "genTest")
	suppressWarnings(dir.create(thePath))
	N <- 5

	# CSV method
	setEctdDataMethod("CSV")
	generateData(N, 2, treatDoses = 0, respEqn = "SUBJ", workingPath = thePath)
	genFiles <- list.files(file.path(thePath, "ReplicateData"))
	checkTrue(length(genFiles) == N & all(casefold(genFiles) == paste("replicate000", 1:N, ".csv", sep="")))

	# RData method
	setEctdDataMethod("RData")
	generateData(N, 2, treatDoses = 0, respEqn = "SUBJ", workingPath = thePath)
	genFiles <- list.files(file.path(thePath, "ReplicateData"))
	checkTrue(length(genFiles) == N & all(casefold(genFiles) == paste("replicate000", 1:N, ".rdata", sep="")))

	# Remove directories
	try(unlink(thePath, recursive = TRUE))

	# Internal method
	setEctdDataMethod("Internal")
	.ectdEnv$DataStore <- NULL
	generateData(N, 2, treatDoses = 0, respEqn = "SUBJ")
	checkTrue(length(.ectdEnv$DataStore) == N)
	.ectdEnv$DataStore <- NULL

	# Finish up
	setEctdDataMethod(nowMethod)
	resetEctdColNames()

}

test.generateData.timevarying <- function() {

	## set up a temp directory as the workingPath
	dir.create( workPath <- tempfile() )
	setEctdDataMethod("CSV")

	# Set up elements of the run
	respFun <- "E0 + (EMAX * DOSE) / (ED50 + DOSE)"

	# Execute call
	resetEctdColNames()
	genCalltime1 <- try(generateData(replicateN = 2, subjects = 500, treatDoses = c(0, 15, 30), treatPeriod = 0:3,
					disCovNames = "DisCov1,DisCov2", disCovVals="1,2#1,2,3", disCovProbArray = rbind(c(.1, .1, .3), c(.3, .1, .1)),
					timeCovNames = "T1,T2", timeCovMean  = list("2.3,2.5,2.9,3.1", rep(5, 4)), timeCovVCov = list(1, 1:4), timeCovCrit = list("T1>0", "T2>0"),
					genParNames = c("E0","ED50","EMAX"), genParMean = c(0, 50, 10), genParVCov=diag(c(1, 0, 0)),
					genParBtwNames = c("E0", "EMAX"), genParBtwVCov = diag(2), genParErrStruc = "None",
					respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteCurrData = FALSE,
					workingPath = workPath))
	resetEctdColNames()

	# Check basics
	checkTrue(class(genCalltime1) != "try-error", msg = "Check the call was successful")
	checkTrue(file.exists( file.path( workPath, "ReplicateData")), msg = "Check ReplicateData directory has been created")
	checkTrue(file.exists( file.path( workPath, "ReplicateData", "replicate0001.csv" ) ), msg = "Check Replicate Data replicate0001.csv was created")
	checkTrue(file.exists( file.path( workPath, "ReplicateData", "replicate0002.csv" ) ), msg = "Check Replicate Data replicate0002.csv was created" )

	# Import the data
	x <- lapply(1:2, readData, dataType="Replicate", workingPath = workPath )
	checkEquals(names(x[[1]])[1:8], c("SUBJ", "TIME", "TRT", "DOSE", "T1", "T2", "DisCov1", "DisCov2" ))

	resetEctdColNames()
	genCalltime2 <- try(generateData(replicateN = 2, subjects = 500, treatDoses = c(0, 15, 30), treatPeriod = 0:3,
					disCovNames = "DisCov1,DisCov2", disCovVals="1,2#1,2,3", disCovProbArray = rbind(c(.1, .1, .3), c(.3, .1, .1)),
					timeCovNames = "T1,T2", timeCovMean  = list("2.3,2.5,2.9", rep(5, 3)), timeCovVCov = list(1, 1:3), timeCovCrit = list("T1>0", "T2>0"),
					genParNames = c("E0","ED50","EMAX"), genParMean = c(0, 50, 10), genParVCov=diag(c(1, 0, 0)),
					genParBtwNames = c("E0", "EMAX"), genParBtwVCov = diag(2), genParErrStruc = "None",
					respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteCurrData = FALSE,
					workingPath = workPath))
	resetEctdColNames()
	checkTrue(inherits(genCalltime2, "try-error"))

	resetEctdColNames()
	genCalltime3 <- try(generateData(replicateN = 2, subjects = 500, treatDoses = c(0, 15, 30),
					disCovNames = "DisCov1,DisCov2", disCovVals="1,2#1,2,3", disCovProbArray = rbind(c(.1, .1, .3), c(.3, .1, .1)),
					timeCovNames = "T1,T2", timeCovMean  = list("2.3,2.5,2.9", rep(5, 3)), timeCovVCov = list(1, 1:3), timeCovCrit = list("T1>0", "T2>0"),
					genParNames = c("E0","ED50","EMAX"), genParMean = c(0, 50, 10), genParVCov=diag(c(1, 0, 0)),
					genParBtwNames = c("E0", "EMAX"), genParBtwVCov = diag(2), genParErrStruc = "None",
					respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteCurrData = FALSE,
					workingPath = workPath))
	resetEctdColNames()
	checkTrue(inherits(genCalltime3, "try-error"))

	resetEctdColNames()
	genCalltime4 <- try(generateData(replicateN = 2, subjects = 500, treatDoses = c(0, 15, 30), treatPeriod = 0:3,
					disCovNames = "DisCov1,DisCov2", disCovVals="1,2#1,2,3", disCovProbArray = rbind(c(.1, .1, .3), c(.3, .1, .1)),
					timeCovNames = "T1,T2", timeCovMean  = list("2.3,2.5,2.9,3.1", rep(5, 4)), timeCovCrit = list("T1>0", "T2>0"),
					genParNames = c("E0","ED50","EMAX"), genParMean = c(0, 50, 10), genParVCov=diag(c(1, 0, 0)),
					genParBtwNames = c("E0", "EMAX"), genParBtwVCov = diag(2), genParErrStruc = "None",
					respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteCurrData = FALSE,
					workingPath = workPath))
	resetEctdColNames()
	x <- lapply(3:4, readData, dataType="Replicate", workingPath = workPath )
	checkEquals(unique(x[[1]]$T1), parseCharInput("2.3,2.5,2.9,3.1"))
	cat(workPath)
	unlink(workPath, recursive = TRUE)
	invisible(NULL)
}

