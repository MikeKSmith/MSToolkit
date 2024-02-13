# Date Jun 30 2007
# Author: Francisco

test_that("test.createDirectories.CSV", {
  setEctdDataMethod("CSV")
  dir.create(tempDir <- tempfile())

  x <- createDirectories(workingPath = tempDir)
  expect_true(all(x),
              info = "all of the directories should be created")
  expect_true(all(file.exists(file.path(tempDir, c("ReplicateData", "MacroEvaluation", "MicroEvaluation")))))

  x <- createDirectories(dirNames = c("Rep", "MacroEvaluation", "Micro"), workingPath = tempDir)
  expect_true(!any(x))
  expect_error(createDirectories(dirName = c(), workingPath = tempDir))

  expect_error(createDirectories(dirNames = letters, workingPath = tempDir))
  try(unlink(tempDir, recursive = TRUE))
})


# Date: Jul 1 2007
# Author: Francisco

test_that("test.createDirectories.RData", {

  setEctdDataMethod("RData")
  dir.create(tempDir <- tempfile())

  x <- createDirectories(workingPath = tempDir)
  expect_true(all(x),
              info = "all of the directories should be created")
  expect_true(all(file.exists(file.path(tempDir, c("ReplicateData", "MacroEvaluation", "MicroEvaluation")))))

  x <- createDirectories(dirNames = c("Rep", "MacroEvaluation", "Micro"), workingPath = tempDir)
  expect_true(!any(x))
  expect_error(createDirectories(dirName = c(), workingPath = tempDir))

  expect_error( createDirectories(dirNames = letters, workingPath = tempDir) )
  try(unlink(tempDir, recursive = TRUE))
})
