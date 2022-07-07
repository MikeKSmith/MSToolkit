test_that("test.getReplicates", {

  # Set test directory
  tempDir <- file.path(tempdir(), "getRepTest")
  if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
  expect_true(dir.create(tempDir))
  expect_true(dir.create(file.path(tempDir, "TestDir")))
  cat("Hello", file = file.path(tempDir, "TestDir", "test0001.csv"))
  cat("Hello", file = file.path(tempDir, "TestDir", "test0001.RData"))
  nowMethod <- getEctdDataMethod()

  # Exceptions
  setEctdDataMethod("CSV")
  expect_error(getReplicates(1))
  expect_error(getReplicates(letters))
  expect_error(getReplicates(prefix = 1))
  expect_error(getReplicates(prefix = LETTERS))
  expect_error(getReplicates(method = 1))
  expect_error(getReplicates(method = letters))
  expect_error(getReplicates(workingPath = 1))
  expect_error(getReplicates(workingPath = letters))
  expect_error(getReplicates(path = "MADEUPPATH", method = "CSV"))
  expect_error(getReplicates(path = "MADEUPPATH", method = "RData"))
  expect_error(getReplicates(workingPath = tempDir))
  expect_error(getReplicates("TestDir", workingPath = tempDir))
  expect_true(getReplicates("TestDir", workingPath = tempDir, prefix = "test") == 1)
  setEctdDataMethod("RData")
  expect_error(getReplicates("TestDir", workingPath = tempDir))
  expect_true(getReplicates("TestDir", workingPath = tempDir, prefix = "test") == 1)

  # CSV Method first
  setEctdDataMethod("CSV")
  if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
  expect_true(dir.create(tempDir))
  generateData(5, 3, respEqn = "DOSE", workingPath = tempDir, treatDoses = 1)
  expect_true(all(getReplicates(workingPath = tempDir) == 1:5))
  unlink(file.path(tempDir, "ReplicateData", "replicate0003.csv"))
  expect_true(all(getReplicates(workingPath = tempDir) == c(1:2, 4:5)))

  # RData method
  setEctdDataMethod("RData")
  if (file.exists(tempDir)) try(unlink(tempDir, recursive = TRUE), silent = TRUE)
  expect_true(dir.create(tempDir))
  generateData(5, 3, respEqn = "DOSE", workingPath = tempDir, treatDoses = 1)
  expect_true(all(getReplicates(workingPath = tempDir) == 1:5))
  unlink(file.path(tempDir, "ReplicateData", "replicate0003.RData"))
  expect_true(all(getReplicates(workingPath = tempDir) == c(1:2, 4:5)))

  # Internal Method
  setEctdDataMethod("Internal")
  .ectdEnv$DataStore <- NULL
  generateData(5, 3, respEqn = "DOSE", workingPath = tempDir, treatDoses = 1)
  expect_true(all(getReplicates(workingPath = tempDir) == 1:5))
  .ectdEnv$DataStore <- NULL

  # Reset method
  setEctdDataMethod(nowMethod)
})
