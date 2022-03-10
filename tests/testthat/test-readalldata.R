if( !exists("unitTestPath")) unitTestPath <- system.file(package = "MSToolkit", "tests")
testDir <- file.path(unitTestPath, "testthat", "testdata.datastorage")

# Author: Francisco
# Date: Jul 3 2007

test_that("test.readAllData.CSV", {

  setEctdDataMethod("CSV")
  tempDir <- file.path(tempdir(), "DataStorageTemp")
  if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
  expect_true(dir.create(tempDir))
  microData <- MSToolkit:::microSummary [1:12,-c(2,13)]
  createDirectories("MicroEvaluation", workingPath = tempDir)

  writeData(microData[1:3,], dataNumber = 1, dataType = "Micro", workingPath=tempDir)
  writeData(microData[4:6,], dataNumber = 2, dataType = "Micro", workingPath=tempDir)
  writeData(microData[7:9,], dataNumber = 3, dataType = "Micro", workingPath=tempDir)
  writeData(microData[10:12,], dataNumber = 4, dataType = "Micro", workingPath=tempDir)
  x <- readAllData(dataType = "Micro", workingPath = tempDir)

  rownames(microData)  <- rownames(x)
  expect_equal(microData, x[,-1],
               info = "checking the readAllData function with MicroEvaluation data")
  expect_true(all(x$Replicate == rep(1:4, each=3)),
              info = "Check subset replicate variable created correctly")

  # Now check it with a subset of data
  x <- readAllData(dataType = "Micro", workingPath = tempDir, replicates = 2:3)

  y <- microData[4:9, ]
  rownames(y) <- rownames(x)
  expect_equal(y, x[,-1],
               info = "checking the readAllData function with a reading of partial MicroEvaluation data")
  expect_true(all(x$Replicate == rep(2:3, each=3)),
              info = "Check subset replicate variable created correctly")
  unlink(tempDir, recursive = TRUE)
})
