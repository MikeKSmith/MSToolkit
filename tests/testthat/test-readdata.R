
# Author: Francisco
# Date: Jul 3 2007

test_that("test.readData.CSV", {

  setEctdDataMethod("CSV")
  datapath <- test_path("testdata.datastorage")
  x <- MSToolkit:::microSummary
  y <- readData(dataType = "Micro", dataNumber = 1,
                workingPath = datapath)

  expect_true(identical(x[1:10,-c(1,2,13)], y[ ,-11]))
  expect_error(readData(dataType = "Micro", dataNumber = 2,
                        workingPath = datapath))
  expect_true(!identical(x[2:11,],
                         readData(dataType = "Micro", dataNumber = 1,
                                  workingPath = datapath)))
  expect_error(readData(dataType = "microeval", dataNumber = 1))

  x <- MSToolkit:::ReplicateSample
  x1 <- x[20:80, ]
  rownames(x1) <- 1:61
  y <- readData(dataType = "Replicate", dataNumber = 10,
                workingPath = datapath)
  expect_error(readData(dataType = "Replicate", dataNumber = 11,
                        workingPath = datapath))
})


test_that("test.readData.RData", {

  nowMethod <- getEctdDataMethod()
  tempDir <- file.path(tempdir(), "DataStorageTemp")
  if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
  expect_true(dir.create(tempDir))
  resetEctdColNames()
  setEctdDataMethod("RData")
  generateData(3, 5, respEqn = "DOSE", workingPath = tempDir, treatDoses = 1:2, idCol = "RDATATEST")
  resetEctdColNames()
  genFiles <- list.files(file.path(tempDir, "ReplicateData"))
  expect_true(all(genFiles == paste("replicate000", 1:3, ".RData", sep="")))
  x <- readData(1, workingPath = tempDir)
  expect_true(nrow(x) == 5)
  expect_true("RDATATEST" %in% names(x))
  x <- readAllData(workingPath = tempDir)
  expect_true(nrow(x) == 3*5)
  expect_true("RDATATEST" %in% names(x))
  if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
  setEctdDataMethod(nowMethod)
})

test_that("test.readData.Internal", {

  nowMethod <- getEctdDataMethod()
  .ectdEnv$DataStore <- NULL
  setEctdDataMethod("Internal")
  resetEctdColNames()
  generateData(3, 5, respEqn = "DOSE", treatDoses = 1:2, idCol = "INTERNALTEST")
  resetEctdColNames()
  expect_true(length(.ectdEnv$DataStore) == 3)
  expect_true(all(sapply(.ectdEnv$DataStore, is.data.frame)))
  expect_true(all(sapply(.ectdEnv$DataStore, nrow) == 5))
  x <- readData(1)
  expect_true(nrow(x) == 5)
  expect_true("INTERNALTEST" %in% names(x))
  x <- readAllData()
  expect_true(nrow(x) == 3*5)
  expect_true("INTERNALTEST" %in% names(x))
  .ectdEnv$DataStore <- NULL
  setEctdDataMethod(nowMethod)
})