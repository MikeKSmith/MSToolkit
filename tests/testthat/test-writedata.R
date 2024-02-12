# if( !exists("unitTestPath")) unitTestPath <- system.file(package = "MSToolkit", "tests")
# testDir <- file.path(unitTestPath, "testthat", "testdata.datastorage")

# Author: Francisco
# Date: Jul 3 2007

test_that("test.writeData.CSV", {

  setEctdDataMethod("CSV")
  tempDir <- file.path(tempdir(), "DataStorageTemp")
  if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
  expect_true(dir.create(tempDir))

  x <- MSToolkit:::ReplicateSample[20:30,]
  createDirectories("ReplicateData", workingPath = tempDir)
  writeData(x, dataNumber = 1001, dataType = "Replicate", workingPath = tempDir)
  expect_true(file.exists(writtenFile <- paste(tempDir, "/ReplicateData/replicate1001.csv", sep="")))
  unlink(tempDir, recursive = TRUE)
})


test_that("test.writeData.RData", {

  nowMethod <- getEctdDataMethod()
  setEctdDataMethod("RData")
  tempDir <- file.path(tempdir(), "DataStorageTemp")
  if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
  expect_true(dir.create(tempDir))
  myDf <- data.frame(X = 1:5, Y = 5:1)
  expect_true(all(createDirectories(workingPath = tempDir)))
  writeData(myDf, 1, "ReplicateData", workingPath = tempDir)
  writeData(myDf, 1, "Micro", workingPath = tempDir)
  writeData(myDf, 1, "Macro", workingPath = tempDir)
  expect_true(list.files(file.path(tempDir,"ReplicateData")) == "replicate0001.RData")
  expect_true(list.files(file.path(tempDir,"MicroEvaluation")) == "micro0001.csv")
  expect_true(list.files(file.path(tempDir,"MacroEvaluation")) == "macro0001.csv")
  inData <- readData(1, workingPath = tempDir)
  expect_true(identical(inData, myDf))
  setEctdDataMethod(nowMethod)
})


test_that("test.writeData.Internal", {

  nowMethod <- getEctdDataMethod()
  setEctdDataMethod("Internal")
  .ectdEnv$DataStore <- NULL
  myDf <- data.frame(X = 1:5, Y = 5:1)
  writeData(myDf, 1, "ReplicateData")
  expect_true(length(.ectdEnv$DataStore) == 1)
  expect_true(identical(.ectdEnv$DataStore[[1]], myDf))
  .ectdEnv$DataStore <- NULL
  setEctdDataMethod(nowMethod)
})