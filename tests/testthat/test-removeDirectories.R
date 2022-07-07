# Date: Jul 1 2007
# Author: Francisco

test_that("test.removeDirectories.CSV", {

  setEctdDataMethod("CSV")
  tempDir <- file.path(tempdir(), "DataStorageTemp")
  if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
  expect_true(dir.create(tempDir))

  createDirectories(workingPath = tempDir )
  removeDirectories(workingPath = tempDir )
  expect_true(length(dir(path = tempDir)) == 0)

  createDirectories(dirNames = c("Rep", "Micro", "Macro"), workingPath = tempDir)
  removeDirectories(dirNames = c("Rep", "Micro", "Macro"), workingPath = tempDir)
  expect_true(length(dir(path = tempDir)) == 0)

  createDirectories(dirNames = c("ReplicateData", "MacroEvaluation"), workingPath = tempDir)
  removeDirectories(dirNames = "ReplicateData", workingPath = tempDir)
  expect_true(length(dir(path=tempDir)) == 1)

  unlink(tempDir, recursive = TRUE)
})


test_that("test.removeDirectories.RData", {

  setEctdDataMethod("RData")
  tempDir <- file.path(tempdir(), "DataStorageTemp")
  if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
  expect_true(dir.create(tempDir))

  createDirectories(workingPath = tempDir)
  removeDirectories(workingPath = tempDir)
  expect_true(length(dir(path = tempDir)) == 0)

  createDirectories(dirNames = c("Rep", "Micro", "Macro"), workingPath = tempDir)
  removeDirectories(dirNames = c("Rep", "Micro", "Macro"), workingPath = tempDir)
  expect_true(length(dir(path = tempDir)) == 0)

  createDirectories(dirNames = c("ReplicateData", "MacroEvaluation"), workingPath = tempDir)
  removeDirectories(dirNames = "ReplicateData", workingPath = tempDir)
  expect_true(length(dir(path=tempDir)) == 1)

  unlink(tempDir, recursive = TRUE)
})
