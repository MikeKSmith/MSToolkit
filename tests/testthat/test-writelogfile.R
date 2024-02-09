test_that("test.writeLogFile", {

  # Create temporary directory
  tempDir <- file.path(tempdir(), "LogFileTemp")
  dir.create(tempDir, showWarnings = FALSE)
  expect_true(file.exists(tempDir))

  # Test1: check range of possible status fields
  testVec1 <- c("PEND", "WAIT", "RUN", "DONE", "EXIT", "PSUSP", "USUSP", "SSUSP", "ZOMBI")
  writeLogFile(testVec1, Sys.time(), workingPath = tempDir)
  x <- readLines(file.path(tempDir, "jobstatus.log"))
  grepProgress <- as.logical(length(grep("Some grid jobs not yet completed", x)))
  expect_true(grepProgress, info = "Check job in progress message")
  timeCheck <- as.numeric(gsub(" ", "", gsub("secs", "", substring(x[3], 36))))
  expect_true(timeCheck >= 0 & timeCheck < 10, info = "Check time since job started")
  jobCheck <- x[-(1:3)]
  jobName <- gsub(" ", "", substring(jobCheck, 1, 15))
  jobNum <- as.numeric(gsub(" ", "", substring(jobCheck, 22, 23)))
  expect_true(all(jobName == c("PEND", "RUN", "DONE", "EXIT", "SUSPEND")),
              info = "Check correct job names and correct order")
  expect_true(all(jobNum == c(2, 1, 1, 1, 4)), info = "Check correct number of jobs of each category")

  # Test2: check invalid values
  testVec2 <- c("PEND", "WAIT", "X", "Y", "Z", "ZOMBI")
  expect_error(writeLogFile(testVec2, Sys.time(), workingPath = tempDir),
               regexp = "Input vector contains unknown status values", 
               fixed = TRUE)
  expect_error(writeLogFile(testVec1, 1:5, workingPath = tempDir),
               regexp = "Must supply a single starting time",
               fixed = TRUE)
  expect_error(writeLogFile(character(0), Sys.time(), workingPath = tempDir),
               regexp = "Input vector is empty", 
               fixed = TRUE)

  # Test3: All jobs completed
  testVec3 <- rep(c("DONE", "EXIT"), each = 10)
  writeLogFile(testVec3, Sys.time(), workingPath = tempDir)
  x <- readLines(file.path(tempDir, "jobstatus.log"))
  grepProgress <- as.logical(length(grep("All jobs completed", x)))
  expect_true(grepProgress, info = "Check all jobs finished message")
  grepSuccess <- as.logical(length(grep("Successful runs: 10", x)))
  expect_true(grepSuccess, info = "Check successful jobs message")
  grepFailure <- as.logical(length(grep("Unsuccessful runs: 10", x)))
  expect_true(grepFailure, info = "Check unsuccessful jobs message")

  try(unlink(tempDir, recursive = TRUE))
})
