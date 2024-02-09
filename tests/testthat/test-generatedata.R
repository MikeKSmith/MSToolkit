setEctdDataMethod("CSV")


covariatesDataFile <- test_path("data","createCovariates" ,"testCovariates.csv")
parametersDataFile <- test_path("data", "createParameters","testParam.csv")
resetEctdColNames()

test_that("test.generateData.call1", {

  ## set up a temp directory as the workingPath
  dir.create(workPath <- tempfile())
  setEctdDataMethod("CSV")

  # Set up elements of the run
  respFun <- "E0 + (EMAX * D) / (ED50 + D)"
  seqMat <- cbind(c(0, 15, 30, 45), c(45, 15, 30, 0) ,c(45, 30, 15, 0))
  dFun <- function(data, prop) ifelse(data$HOUR <= 0, 0, sample(0:1, nrow(data), TRUE, c(1-prop, prop)))

  # Create a dummy file (to check if it is overwritten)
  dir.create(file.path(workPath, "ReplicateData"), showWarnings = FALSE)
  write.csv(data.frame(X=1:3, Y=1:3), file.path(workPath, "ReplicateData", "replicate0003.csv"))

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
                               workingPath = workPath, mcarRule = "HOUR > 0"))
  resetEctdColNames()

  # Check basics
  expect_true(class(genCall1) != "try-error", info = "Check the call was successful")
  expect_true(file.exists(file.path(workPath, "ReplicateData") ),
              info = "Check ReplicateData directory has been created")
  expect_true(file.exists(file.path(workPath, "ReplicateData", "replicate0001.csv")),
              info = "Check Replicate Data replicate0001.csv was created")
  expect_true(file.exists(file.path(workPath, "ReplicateData", "replicate0002.csv")),
              info = "Check Replicate Data replicate0002.csv was created" )
  expect_true(!file.exists(file.path(workPath, "ReplicateData", "replicate0003.csv")),
              info = "Check Replicate Data replicate0003.csv was removed")

  # Import the data
  x <- lapply(1:2, readData, dataType="Replicate", workingPath = workPath )

  # Check variables exist in the data
  expect_true(all(c("SUBJ", "T", "HOUR", "D") %in% names(x[[1]])),
              info = "Check dosing variables are in the data")
  expect_true(all(c("ConCov1", "ConCov2", "ConCov3", "DisCov1", "DisCov2", "DisCov3") %in% names(x[[1]])),
              info = "Check covariate variables are in the data")
  expect_true(all(c("E0", "ED50", "EMAX", "PARFLAG") %in% names(x[[1]])),
              info = "Check parameter variables are in the data")
  expect_true(all(c("MyResponse", "RESPFLAG", "MISSFLAG", "INT") %in% names(x[[1]])),
              info = "Check response variables are in the data")

  # Check dosing regimes set up
  theTreats <- unique(x[[1]][,c("T", "HOUR", "D")])
  theTreats <- theTreats[order(theTreats$T, theTreats$HOUR, theTreats$D),]
  expect_true(nrow(theTreats) == 15 && all(theTreats$T == rep(1:3, each=5)),
              info = "Check treatment column (T)")
  expect_true(nrow(theTreats) == 15 && all(theTreats$HOUR == rep(c(0, 1:3, 5), 3)),
              info = "Check time column (HOUR)")
  expect_true(nrow(theTreats) == 15 && all(theTreats$D == as.vector(rbind(0, seqMat))),
              info = "Check dose column (D)")

  # Check treatment allocation
  x1 <- unique(x[[1]][,c("SUBJ", "T")])
  x2 <- unique(x[[2]][,c("SUBJ", "T")])
  x2$SUBJ <- x2$SUBJ + 500
  treatAlloc <- rbind(x1, x2)
  expect_true(nrow(treatAlloc) == 1000,
              info = "Check all subjects returned and allocated")
  expect_true(binom.test(table(treatAlloc$T != 1), p=.2)$p.value > 0.05,
              info = "Check correct proportional allocation of treatment 1")
  #expect_true(binom.test(table(treatAlloc$T != 2), p=.2)$p.value > 0.05,
  #            info = "Check correct proportional allocation of treatment 2")
  expect_true(binom.test(table(treatAlloc$T != 3), p=.6)$p.value > 0.05,
              info = "Check correct proportional allocation of treatment 3")
  expect_true(!all(x1$T == x2$T),
              info = "Check continuous treatments differ between replicates")

  # Check continuous covariates
  conData1 <- x[[1]][!duplicated(x[[1]]$SUBJ),c("ConCov1", "ConCov2", "ConCov3")]
  conData2 <- x[[2]][!duplicated(x[[1]]$SUBJ),c("ConCov1", "ConCov2", "ConCov3")]
  expect_true(!all(conData1$ConCov1 == conData2$ConCov1),
              info = "Check covariates differ between replicates")
  expect_true(all(conData1$ConCov1 >= 0),
              info = "Check all of covariate 1 are greater than 0 (as specified in the criteria)")
  expect_true(t.test(conData1$ConCov2)$p.value > 0.05,
              info = "Check distribution of covariate 2")
  expect_true(t.test(conData1$ConCov3)$p.value > 0.05,
              info = "Check distribution of covariate 3")
  expect_true(cor.test(conData1$ConCov2, conData1$ConCov3)$p.value < 0.05,
              info = "Check for (lack of) correlation between covariates")

  # Check discrete covariates: disCovVals=list(0:1,1:2,1:3), disCovProb = ".5,.5#.5,.5#.3,.3,.4",
  disData1 <- x[[1]][!duplicated(x[[1]]$SUBJ),c("DisCov1", "DisCov2", "DisCov3")]
  disData2 <- x[[2]][!duplicated(x[[1]]$SUBJ),c("DisCov1", "DisCov2", "DisCov3")]
  expect_true(!all(disData1$DisCov1 == disData2$DisCov1),
              info = "Check discrete covariates differ between replicates")
  expect_true(all(disData1$DisCov1 %in% 0:1),
              info = "Check values for discrete covariate 1")
  expect_true(all(disData1$DisCov2 %in% 1:2),
              info = "Check values for discrete covariate 2")
  expect_true(all(disData1$DisCov3 %in% 1:3),
              info = "Check values for discrete covariate 3")
  expect_true(all(binom.test(table(disData1$DisCov1 != 1), p=.5)$p.value >.05),
              info = "Check proportion for discrete covariate 1")
  expect_true(all(binom.test(table(disData1$DisCov2 != 1), p=.5)$p.value >.05),
              info = "Check proportion for discrete covariate 1")
  expect_true(all(binom.test(table(disData1$DisCov3 != 1), p=.3)$p.value >.05),
              info = "Check proportion for discrete covariate 1 (value 1)")
  expect_true(all(binom.test(table(disData1$DisCov3 != 3), p=.4)$p.value >.05),
              info = "Check proportion for discrete covariate 1 (value 3)")

  # Check parameters
  parData <- x[[1]][,c("SUBJ", "E0", "ED50", "EMAX")]
  expect_true(all(parData$ED50 == parData$ED50[1]),
              info = "Check all ED50s are the same (no random effects)")
  expect_true(!all(parData$E0 == parData$E0[1]),
              info = "Check E0 effects differ between subject")
  expect_true(!all(parData$EMAX == parData$EMAX[1]),
              info = "Check EMAX effects differ between subject")
  expect_true(all(tapply(parData$EMAX, parData$SUBJ, function(x) all(x == x[1]))),
              info = "Check effects are the same within subject")
  parData <- parData[!duplicated(parData$SUBJ),]
  expect_true(t.test(parData$EMAX, mu = 10)$p.value > 0.05,
              info = "Check distribution of EMAX")

  # Check response
  respData <- x[[1]][,c("E0", "ED50", "EMAX", "D", "MyResponse", "RESPFLAG")]
  respData$PRED <- with(respData, E0 + (EMAX * D) / (ED50 + D))
  respData$RES <- respData$MyResponse - respData$PRED
  expect_true(t.test(respData$RES[1:500])$p.value > .05,
              info = "Check response variable has been created correctly")
  respTab <- table(respData$RESPFLAG, cut(respData$MyResponse, c(-100, 0, 6, 100)))
  expect_true(nrow(respTab) == 2 & ncol(respTab) == 3 & sum(respTab) == nrow(respData),
              info = "Check values have been generated outside the range")
  expect_true(respTab[1,1] == 0 & respTab[2,2] == 0 & respTab[1,3] == 0,
              info = "Check values outside response range have been flagged as omitted")

  # Check missings
  missTab <- table(x[[1]]$HOUR, x[[1]]$MISSFLAG)
  expectMCAR <- c(0, rep(.05*500, 4))
  expectDrop <- 0:4 * 0.05 * 475
  missTab <- cbind(missTab, MCAR = expectMCAR, DROP = expectDrop, EXPECT = expectMCAR + expectDrop )
  expect_true(all(apply(missTab, 1, function(x) binom.test(x[2:1], p=x[5]/500)$p.value) > .05),
              info = "Checking actual vs expected missing proportions")

  # Check interim data
  intData <- x[[1]][,c("SUBJ", "INT")]
  expect_true(all(tapply(intData$INT, intData$SUBJ, function(x) all(x == x[1]))),
              info = "Check interims allocated are the same within subject")
  intData <- intData[!duplicated(intData$SUBJ),]
  expect_true(all(intData$INT %in% 1:4), info = "Check values of interims allocated")
  expect_true(binom.test(table(intData$INT != 1), p = .3)$p.value > 0.05,
              info = "Check proportions of interims allocated (interim 1)")
  expect_true(binom.test(table(intData$INT != 2), p = .3)$p.value > 0.05,
              info = "Check proportions of interims allocated (interim 2)")
  expect_true(binom.test(table(intData$INT != 3), p = .2)$p.value > 0.05,
              info = "Check proportions of interims allocated (interim 3)")
  expect_true(binom.test(table(intData$INT != 4), p = .2)$p.value > 0.05,
              info = "Check proportions of interims allocated (interim 4)")

  unlink(workPath, recursive = TRUE)
  invisible(NULL)
})


test_that("test.generateData.call2", {

  ## set up a temp directory as the workingPath
  dir.create(workPath <- tempfile())
  setEctdDataMethod("CSV")

  # Set up elements of the run
  respFun <- "E0 + (EMAX * DOSE) / (ED50 + DOSE)"

  # Create a dummy file (to check if it is overwritten)
  dir.create(file.path(workPath, "ReplicateData") , showWarnings = FALSE)
  write.csv(data.frame(X=1:3, Y=1:3), file.path(workPath, "ReplicateData", "replicate0003.csv"))

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
  expect_true(class(genCall2) != "try-error",
              info = "Check the call was successful")
  expect_true(file.exists(file.path(workPath, "ReplicateData")),
              info = "Check ReplicateData directory has been created")
  expect_true(!file.exists(file.path(workPath, "ReplicateData", "replicate0001.csv" )),
              info = "Check Replicate Data replicate0001.csv was created")
  expect_true(!file.exists(file.path(workPath, "ReplicateData", "replicate0002.csv" )),
              info = "Check Replicate Data replicate0002.csv was created")
  expect_true(file.exists(file.path(workPath, "ReplicateData", "replicate0003.csv" )),
              info = "Check (dummy) Replicate Data replicate0003.csv still exists")
  expect_true(file.exists(file.path(workPath, "ReplicateData", "replicate0004.csv" )),
              info = "Check Replicate Data replicate0001.csv was created")
  expect_true(file.exists(file.path(workPath, "ReplicateData", "replicate0005.csv" )),
              info = "Check Replicate Data replicate0002.csv was created")

  # Import the data
  x <- lapply(4:5, readData, dataType="Replicate", workingPath = workPath)

  # Check variables exist in the data
  expect_true(all(c("SUBJ", "TIME", "DOSE", "TRT") %in% names(x[[1]])),
              info = "Check dosing variables are in the data")
  expect_true(all(c("DisCov1", "DisCov2") %in% names(x[[1]])),
              info = "Check covariate variables are in the data")
  expect_true(all(c("E0", "ED50", "EMAX", "E0.Extra", "EMAX.Extra", "RESP") %in% names(x[[1]])),
              info = "Check parameter variables are in the data")

  # Check dosing regimes set up
  theTreats <- unique(x[[1]][,c("TRT", "TIME", "DOSE")])
  theTreats <- theTreats[order(theTreats$TRT, theTreats$TIME, theTreats$DOSE), ]

  expect_true(nrow(theTreats) == 12 && all(theTreats$TRT == rep(1:3, each=4)),
              info = "Check treatment column (TRT)")
  expect_true(nrow(theTreats) == 12 && all(theTreats$TIME == rep(0:3, 3)),
              info = "Check time column (TIME)")
  expectDose <- c(rep(0, 4), rep(15, 4), rep(30, 4))
  expect_true(nrow(theTreats) == 12 && all(theTreats$DOSE == expectDose),
              info = "Check dose column (D)")

  # Check treatment allocation
  x1 <- unique(x[[1]][,c("SUBJ", "TRT")])
  x2 <- unique(x[[2]][,c("SUBJ", "TRT")])
  expect_true(all(x1$T == x2$T), info = "Check continuous treatments differ between replicates")

  # Check discrete covariates
  disData1 <- x[[1]][!duplicated(x[[1]]$SUBJ),c("DisCov1", "DisCov2")]
  disData2 <- x[[2]][!duplicated(x[[1]]$SUBJ),c("DisCov1", "DisCov2")]
  expect_true(all(disData1$DisCov1 == disData2$DisCov1),
              info = "Check discrete covariates are the same between replicates")
  expect_true(all(disData1$DisCov1 %in% 1:2),
              info = "Check values for discrete covariate 1")
  expect_true(all(disData1$DisCov2 %in% 1:3),
              info = "Check values for discrete covariate 2")
  expect_true(binom.test(table(disData1$DisCov1 != 1), p = .5)$p.value > .05,
              info = "Check overall proportion for covariate 1")
  expect_true(binom.test(table(disData1$DisCov2 != 1), p = .4)$p.value > .05,
              info = "Check overall proportion for covariate 2")
  disTest1 <- disData1$DisCov1 == 1 & disData1$DisCov2 == 1
  disTest2 <- disData1$DisCov1 == 2 & disData1$DisCov2 == 1
  expect_true(binom.test(table(!disTest1), p = .1)$p.value > .05,
              info = "Check multinomial proportion (1,1)")
  expect_true(binom.test(table(!disTest2), p = .3)$p.value > .05,
              info = "Check multinomial proportion (2,1)")

  # Check parameters
  parData <- x[[1]][,c("SUBJ", "E0", "ED50", "EMAX", "E0.Extra", "EMAX.Extra")]
  expect_true(all(parData$ED50 == 50), info = "Check all ED50s are the same")
  expect_true(all(parData$E0 == parData$E0[1]), info = "Check all E0s are the same")
  expect_true(all(parData$EMAX == 10), info = "Check all EMAXs are the same")
  expect_true(!all(parData$E0.Extra == parData$E0.Extra[1]), info = "Check all E0.Extras are different")
  expect_true(!all(parData$EMAX.Extra == parData$EMAX.Extra[1]), info = "Check all EMAX.Extras are different")
  expect_true(abs(parData$E0[1]) < 4, info = "Check E0 is in expected range")
  expect_true(all(tapply(parData$EMAX.Extra, parData$SUBJ, function(x) all(x == x[1]))),
              info = "Check effects are the same within subject")
  parData <- parData[!duplicated(parData$SUBJ), c("E0.Extra", "EMAX.Extra")]
  expect_true(t.test(parData$EMAX.Extra)$p.value > 0.05, info = "Check distribution of EMAX.Extra")

  # Check response
  respData <- x[[1]][,c("E0", "ED50", "EMAX", "DOSE", "RESP")]
  respData$PRED <- with(respData, E0 + (EMAX * DOSE) / (ED50 + DOSE))
  respData$RES <- respData$RESP - respData$PRED
  expect_true(all(round(respData$RES, 3) == 0), info = "Check response variable has been created correctly")

  unlink(workPath, recursive = TRUE)
  invisible(NULL)
})


test_that("test.generateData.call3", {

  dir.create(td3 <- tempfile())
  file.copy(covariatesDataFile, td3)
  file.copy(parametersDataFile, td3)

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
  expect_true(class(genCall3) != "try-error", info = "Check call 3 was successful")
  expect_true(class(genCall4) != "try-error", info = "Check call 4 was successful")

  # Check covariates
  expect_true(all(x3$X3 %in% 1:10), info = "Check imported covariate values")
  expect_true(all(x3$X1 == x3$X2), info = "Check all covariates taken from same line")
  expect_true(!all(x4$X1 == x4$X2), info = "Check all covariates not taken from same line")
  expect_true(all(x3$X1 == x3$ID.refCol), info = "Check reference column correctly created")
  expect_true(all(x4$X2 >= 0), info = "Check covariate subset works")

  # Check Parameters
  expect_true(all(c("B1", "B2") %in% names(x3)), info = "Check between subject variables imported correctly")
  expect_true(!any(c("B1", "B2") %in% names(x4)), info = "Check between subject variables removed correctly")
  expect_true(all(round(x3$E0 + x3$B1 - x4$E0, 3) == 0), info = "Check random effects correctly applied")
  expect_true(all(x3$B1 >= 0), info = "Check between subject subset applied correctly")

  # Check refCol in parameters
  importPars <- read.csv(parametersDataFile, sep=",", header=TRUE)
  importPars <- importPars[!duplicated(importPars$ID), c("ID", "B1", "B2")]
  
  names(importPars) <- c("ID", "Orig1", "Orig2")
  importPars <- merge(importPars, x5[,c("ID.refCol", "B1", "B2")], by.x="ID", by.y="ID.refCol")
  expect_true(all(round(importPars$B1 - importPars$Orig1, 3) == 0), info = "Check reference column functionality")

  try( unlink(td3, recursive = TRUE) )
  invisible(NULL)
})

### change request 29aug07
### Wed Aug 29 11:04:59 BST 2007 @461 /Internet Time/
# testing that the default covariance works for generateData call
test_that("test.changeRequest19aug07", {

  ## set up a temp directory as the workingPath
  dir.create(workPath <- tempfile())

  # Set up elements of the run
  respFun <- "E0 + (EMAX * DOSE) / (ED50 + DOSE)"

  # Create a dummy file (to check if it is overwritten)
  dir.create(file.path(workPath, "ReplicateData") , showWarnings = FALSE)

  # Execute call 2
  genCall1 <- try(generateData(replicateN = 2, subjects = 500, treatDoses = c(0, 15, 30), treatPeriod = 0:3,
                               disCovNames = "DisCov1,DisCov2", disCovVals="1,2#1,2,3", disCovProbArray = rbind(c(.1, .1, .3), c(.3, .1, .1)),
                               genParNames = c("E0","ED50","EMAX"), genParMean = c(0, 50, 100) , genParErrStruc = "None",
                               respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteCurrData = FALSE,
                               workingPath = workPath))
  resetEctdColNames()

  expect_true(class(genCall1) != "try-error", info = "Check call was successful")

  x <- do.call(rbind, lapply(1:2, readData, dataType="Replicate", workingPath = workPath ))

  expect_true(all( x[, "E0"] == 0), info = "check default vcov matrix to 0 ()")
  expect_true(all( x[, "ED50"] == 50), info = "check default vcov matrix to 0 ()")
  expect_true(all( x[, "EMAX"] == 100), info = "check default vcov matrix to 0 ()")
})


test_that("test.generateData.testDelimiters", {

  # Set up path and files
  dir.create(testDelims <- tempfile())
  otherFiles <- c("testParam.nonmem", "testParam.tabDelim", "testParam.spaceDelim")

  # Try a high level call for each file type
  for (i in otherFiles) {
    file.copy( test_path("systemTest","data", i), testDelims)
    genCall3 <- try(generateData(replicateN = 1, subjects = 10, treatDoses = 1,
                                 extParNames = "E0,ED50,EMAX", extParBtwNames = "B1,B2", extParBtwNums = c(1, 3),
                                 extParErrStruc = "None", extParDataId = "ID",
                                 extParFile = i, extParSubset = "B1 > 0",
                                 respEqn = "E0 + ED50 + EMAX", treatDiff = FALSE, seed = 1,
                                 parBtwSuffix=".Extra", deleteCurrData = FALSE, workingPath = testDelims))

    x <- readData(1, dataType="Replicate", workingPath = testDelims)
    resetEctdColNames()

    expect_true(all(x$RESP == 148.722), info = paste("Check parameter data correctly imported from", i))
  }
  try(unlink(testDelims, recursive = TRUE))
  invisible(NULL)
})



test_that("test.generateData.Version2.0", {

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
  expect_true(length(genFiles) == 1 && genFiles == "replicate0001.csv")
  getData <- readAllData(workingPath = thePath)
  expect_true(nrow(getData) == 5)
  expect_true(all(getData$Replicate == 1) & all(getData$SUBJ == getData$RESP) & all(getData$SUBJ == 1:5))
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
  expect_true(identical(getData1, getData2))

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
  expect_error(generateData(1, 10, treatDoses = c(0, 15), treatPeriod = 1:2,
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
  expect_true( all(expectNames %in% names(getData1)) )
  expect_true( !any(expectNames %in% names(getData2)) )

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
  expect_true( length(genFiles) == 2 )
  generateData(2, 5, treatDoses = 0, respEqn = "SUBJ", workingPath = thePath, deleteCurrData = FALSE)
  genFiles <- list.files(file.path(thePath, "ReplicateData"))
  expect_true(  length(genFiles) == 4 )
  generateData(2, 5, treatDoses = 0, respEqn = "SUBJ", workingPath = thePath)
  genFiles <- list.files(file.path(thePath, "ReplicateData"))
  expect_true ( length(genFiles) == 2 )

  # RData method
  setEctdDataMethod("RData")
  generateData(2, 5, treatDoses = 0, respEqn = "SUBJ", workingPath = thePath)
  genFiles <- list.files(file.path(thePath, "ReplicateData"))
  expect_true(length(genFiles) == 2)
  generateData(2, 5, treatDoses = 0, respEqn = "SUBJ", workingPath = thePath, deleteCurrData = FALSE)
  genFiles <- list.files(file.path(thePath, "ReplicateData"))
  expect_true(length(genFiles) == 4)
  generateData(2, 5, treatDoses = 0, respEqn = "SUBJ", workingPath = thePath)
  genFiles <- list.files(file.path(thePath, "ReplicateData"))
  expect_true(length(genFiles) == 2)

  # Remove directories
  try(unlink(thePath, recursive = TRUE))

  # Internal method
  setEctdDataMethod("Internal")
  generateData(2, 5, treatDoses = 0, respEqn = "SUBJ")
  expect_true(length(.ectdEnv$DataStore) == 2)
  generateData(2, 5, treatDoses = 0, respEqn = "SUBJ", deleteCurrData = FALSE)
  expect_true(length(.ectdEnv$DataStore) == 4)
  generateData(2, 5, treatDoses = 0, respEqn = "SUBJ")
  expect_true(length(.ectdEnv$DataStore) == 2)

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

  expect_true(identical(splitData1[[1]], splitData1[[2]]))

  # Tests: 2nd data example should not have same covariates in each replicate
  splitData2 <- split(getData2[LETTERS[1:3]], getData2$Replicate)
  splitData2 <- lapply(splitData2, function(df) {
    row.names(df) <- as.character(1:nrow(df))
    df
  })
  expect_true(!identical(splitData2[[1]], splitData2[[2]]))

  # Tests: Check distribution of generated covariates
  getData <- getData2[LETTERS[1:3]]
  statTest1 <- t.test(getData$A, mu = 5)$p.value > .025
  statTest2 <- t.test(getData$B, mu = 6)$p.value > .025
  statTest3 <- t.test(getData$C, mu = 7)$p.value > .025
  expect_true(all(statTest1, statTest2, statTest3))
  expect_true(all(getData$C >= 5 & getData$C <= 9))
  expect_true(var(getData$B) > var(getData$A))

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

  expect_true(all(getData1$A %in% 0:1) & all(getData1$B %in% 1:4) & all(getData1$C %in% c(15, 30)))
  expect_true(prop.test(aTable[1], N, .5)$p.value > .01)
  expect_true(prop.test(bTable[1], N, .5)$p.value > .01)
  expect_true(prop.test(cTable[2], N, .5)$p.value > .01)

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
  expect_true(sum(pVals) >= 5)

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
  expect_true(all(1:3 %in% splitDose[[1]]))
  expect_true(!all(splitDose[[1]] == splitDose[[2]]))
  expect_true(all(sort(splitDose[[1]]) == splitDose[[2]]))
  expect_true(all(as.vector(table(splitDose[[2]])) == c(10, 5, 5)))

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
  expect_true(nrow(uniData) == N * 4)  # Should be parallel - only 1 dose per ID
  splitDose <- split(uniData$DOSE, uniData$Replicate)
  expect_true(!all(splitDose[[1]] == splitDose[[2]]))
  expect_true(all(splitDose[[3]] == splitDose[[4]]))
  allDoses <- getData$DOSE [ getData$Replicate %in% 1:2 & getData$TIME == 1 ]
  doseTable <- as.vector(table(allDoses))
  expect_true(prop.test(doseTable, rep(N*2, 3), c(.5, .25, .25))$p.value > .005)

  # Testing crossover design and run-in
  N <- 20
  generateData(1, N, treatPeriod = -1:3, treatDoses = 1:3,
               respEqn = "SUBJ", treatDiff = TRUE)
  getData <- readAllData()
  expect_true(all(getData$DOSE [ getData$TIME < 0 ] == 0))
  expect_true(!any(getData$DOSE [ getData$TIME >= 0 ] == 0))
  generateData(1, N, treatSeq = cbind(c(0, 15, 30), c(15, 30, 0) ,c(30, 15, 0)),
               treatPeriod = -1:2, respEqn = "SUBJ", treatDiff = TRUE)
  getData <- readAllData()
  expect_true(all(getData$DOSE [ getData$TIME < 0 ] == 0))
  getData <- getData [ getData$TIME >= 0, ]
  allDoses <- sapply(split(getData$DOSE, getData$SUBJ), paste, collapse=", ")
  expect_true(all(allDoses %in% c("0, 15, 30", "15, 30, 0", "30, 15, 0")))

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
  expect_true(prop.test(splitDose, rep(N, 6), probVec)$p.value > .01)

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
  expect_true(nrow(getData) == N)
  expect_true(t.test(getData$A, mu = 1)$p.value > .01)
  expect_true(t.test(getData$B, mu = 2)$p.value > .01)
  expect_true(t.test(getData$C, mu = 3)$p.value > .01)

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
  expect_true(nrow(getData1) == N * 5 * 3 & nrow(getData2) == N * 5 * 3)
  expect_true(nrow(unique(getData1[c("Replicate", "B")])) == 5)
  expect_true(nrow(unique(getData1[c("Replicate", "A")])) == N * 5)
  expect_true(nrow(unique(getData1[c("Replicate", "C")])) == N * 5)
  expect_true(nrow(unique(getData2[c("Replicate", "A", "B", "C")])) == 5)
  btwNames <- paste(c("A", "C"), "Between", sep=".")
  expect_true(!any(btwNames %in% names(getData1)) & all(btwNames %in% names(getData2)))
  expect_true(all(errDiff == 0))
  getData2 <- getData2 [ !duplicated(getData2[c("Replicate", "SUBJ")]), ]
  btwVals <- c(getData2$A.Between, getData2$C.Between)
  expect_true(t.test( btwVals, mu = 10)$p.value > .025)

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
  expect_true(with(getData, all(RESP == DOSE + SUBJ + 1)))
  respFun <- function(data) with(data, DOSE + SUBJ + 1)
  generateData(1, 5, treatDoses = 15, respEqn = respFun)
  getData <- readAllData()
  expect_true(with(getData, all(RESP == DOSE + SUBJ + 1)))

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
  expect_true(all(with(allData, round(ADD, 3) == round(ID + ERR, 3))))
  expect_true(all(with(allData, round(PROP, 2) == round(ID * (1 + ERR), 2))))
  expect_true(all(with(allData, round(LOGN, 2) == round(ID * exp(ERR), 2))))

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
  expect_true(with(getData, !any(TIME [ Replicate == 3 & MISSING == 1] < 2)))
  p10 <- sum(getData$MISSING [ getData$Replicate == 1 ])
  p50 <- sum(getData$MISSING [ getData$Replicate == 2 ]) + sum(getData$MISSING [ getData$Replicate == 3 & getData$TIME > 1 ])
  n10 <- sum(getData$Replicate == 1)
  n50 <- sum(getData$Replicate == 2) + sum(getData$Replicate == 3 & getData$TIME > 1)
  expect_true(prop.test(c(p10, p50), c(n10, n50), c(.1, .5))$p.value > .01)

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
  expect_true(all(R1$MISSING [ R1$TIME > 2] == 1))
  expect_true(all(R1$MISSING [ R1$TIME <= 2] == 0))
  expect_true(all(R2$MISSING [ R2$TIME <= 1] == 0))
  checkDirection <- tapply(R2$MISSING, R2$SUBJ, function(vec) all(diff(vec) >= 0))
  expect_true(all(checkDirection))

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
  expect_true(nrow(uniData) == 2 * 100)
  aggData <- aggregate(list(N = uniData$Replicate), uniData[c("INTERIM", "Replicate")], length)
  sumData <- aggregate(list(TOTAL = uniData$Replicate), uniData["Replicate"], length)
  mData <- merge(aggData, sumData)

  probData <- data.frame( Replicate = c(1, 1, 2, 2, 2), INTERIM = c(1:2, 1:3), PROB = c(.5, .5, .2, .5, .3))
  mData <- merge(mData, probData)
  expect_true(prop.test(mData$N, mData$TOTAL, mData$PROB)$p.value > .01)

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
  expect_true(length(genFiles) == N & all(casefold(genFiles) == paste("replicate000", 1:N, ".csv", sep="")))

  # RData method
  setEctdDataMethod("RData")
  generateData(N, 2, treatDoses = 0, respEqn = "SUBJ", workingPath = thePath)
  genFiles <- list.files(file.path(thePath, "ReplicateData"))
  expect_true(length(genFiles) == N & all(casefold(genFiles) == paste("replicate000", 1:N, ".rdata", sep="")))

  # Remove directories
  try(unlink(thePath, recursive = TRUE))

  # Internal method
  setEctdDataMethod("Internal")
  .ectdEnv$DataStore <- NULL
  generateData(N, 2, treatDoses = 0, respEqn = "SUBJ")
  expect_true(length(.ectdEnv$DataStore) == N)
  .ectdEnv$DataStore <- NULL

  # Finish up
  setEctdDataMethod(nowMethod)
  resetEctdColNames()
})


test_that("test.generateData.timevarying", {

  ## set up a temp directory as the workingPath
  dir.create(workPath <- tempfile())
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
  expect_true(class(genCalltime1) != "try-error",
              info = "Check the call was successful")
  expect_true(file.exists(file.path( workPath, "ReplicateData")),
              info = "Check ReplicateData directory has been created")
  expect_true(file.exists(file.path(workPath, "ReplicateData", "replicate0001.csv")),
              info = "Check Replicate Data replicate0001.csv was created")
  expect_true(file.exists( file.path(workPath, "ReplicateData", "replicate0002.csv")),
              info = "Check Replicate Data replicate0002.csv was created" )

  # Import the data
  x <- lapply(1:2, readData, dataType="Replicate", workingPath = workPath )
  expect_equal(c("SUBJ", "TIME", "TRT", "DOSE", "T1", "T2", "DisCov1", "DisCov2" ), names(x[[1]])[1:8])

  resetEctdColNames()
  
 
  expect_error(generateData(replicateN = 2, subjects = 500, treatDoses = c(0, 15, 30), treatPeriod = 0:3,
                            disCovNames = "DisCov1,DisCov2", disCovVals="1,2#1,2,3", disCovProbArray = rbind(c(.1, .1, .3), c(.3, .1, .1)),
                            timeCovNames = "T1,T2", timeCovMean  = list("2.3,2.5,2.9", rep(5, 3)), timeCovVCov = list(1, 1:3), timeCovCrit = list("T1>0", "T2>0"),
                            genParNames = c("E0","ED50","EMAX"), genParMean = c(0, 50, 10), genParVCov=diag(c(1, 0, 0)),
                            genParBtwNames = c("E0", "EMAX"), genParBtwVCov = diag(2), genParErrStruc = "None",
                            respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteCurrData = FALSE,
                            workingPath = workPath),
               regexp = "the length of `treatPeriod` must be equal to the number of time points")

   
  resetEctdColNames()
  expect_error(generateData(replicateN = 2, subjects = 500, treatDoses = c(0, 15, 30),
                           disCovNames = "DisCov1,DisCov2", disCovVals="1,2#1,2,3", disCovProbArray = rbind(c(.1, .1, .3), c(.3, .1, .1)),
                           timeCovNames = "T1,T2", timeCovMean  = list("2.3,2.5,2.9", rep(5, 3)), timeCovVCov = list(1, 1:3), timeCovCrit = list("T1>0", "T2>0"),
                           genParNames = c("E0","ED50","EMAX"), genParMean = c(0, 50, 10), genParVCov=diag(c(1, 0, 0)),
                           genParBtwNames = c("E0", "EMAX"), genParBtwVCov = diag(2), genParErrStruc = "None",
                           respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteCurrData = FALSE,
                           workingPath = workPath),
               regexp = "`treatPeriod` is required when creating time-varying covariates")

  resetEctdColNames()
  expect_warning(generateData(replicateN = 2, subjects = 500, treatDoses = c(0, 15, 30), treatPeriod = 0:3,
                                   disCovNames = "DisCov1,DisCov2", disCovVals="1,2#1,2,3", disCovProbArray = rbind(c(.1, .1, .3), c(.3, .1, .1)),
                                   timeCovNames = "T1,T2", timeCovMean  = list("2.3,2.5,2.9,3.1", rep(5, 4)), timeCovCrit = list("T1>0", "T2>0"),
                                   genParNames = c("E0","ED50","EMAX"), genParMean = c(0, 50, 10), genParVCov=diag(c(1, 0, 0)),
                                   genParBtwNames = c("E0", "EMAX"), genParBtwVCov = diag(2), genParErrStruc = "None",
                                   respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteCurrData = FALSE,
                                   workingPath = workPath),
               regexp = "there is only 1 covariance matrix, use it for all the time point")
  
  resetEctdColNames()
  unlink(file.path(workPath, "ReplicateData"), recursive = TRUE)
  suppressWarnings(generateData(replicateN = 2, subjects = 500, treatDoses = c(0, 15, 30), treatPeriod = 0:3,
               disCovNames = "DisCov1,DisCov2", disCovVals="1,2#1,2,3", disCovProbArray = rbind(c(.1, .1, .3), c(.3, .1, .1)),
               timeCovNames = "T1,T2", timeCovMean  = list("2.3,2.5,2.9,3.1", rep(5, 4)),
               timeCovCrit = list("T1>0", "T2>0"), timeCovVCov = 0,
               genParNames = c("E0","ED50","EMAX"), genParMean = c(0, 50, 10), genParVCov=diag(c(1, 0, 0)),
               genParBtwNames = c("E0", "EMAX"), genParBtwVCov = diag(2), genParErrStruc = "None",
               respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteCurrData = FALSE,
               workingPath = workPath))
  
  x <- readData(dataNumber = 1, dataType="Replicate", workingPath = workPath)
  expect_equal(parseCharInput("2.3,2.5,2.9,3.1"), unique(x$T1))
  cat(workPath)
  unlink(file.path(workPath), recursive = TRUE)
  invisible(NULL)
})
