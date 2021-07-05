
# Date Jul 18 2007
# Author: Richard Pugh
test.writeLogFile <- function()
{
  # Create temporary directory
  tempDir <- file.path(tempdir(), "LogFileTemp")
  dir.create(tempDir, showWarnings = FALSE)
  checkTrue(file.exists(tempDir))
  
  # Test1: check range of possible status fields
  testVec1 <- c("PEND", "WAIT", "RUN", "DONE", "EXIT", "PSUSP", "USUSP", "SSUSP", "ZOMBI")
  writeLogFile(testVec1, Sys.time(), workingPath = tempDir)
  x <- readLines(file.path(tempDir, "jobstatus.log"))
  grepProgress <- as.logical(length(grep("Some grid jobs not yet completed", x)))
  checkTrue(grepProgress, msg = "Check job in progress message")
  timeCheck <- as.numeric(gsub(" ", "", gsub("secs", "", substring(x[3], 36))))
  checkTrue(timeCheck >= 0 & timeCheck < 10, msg = "Check time since job started")
  jobCheck <- x[-(1:3)]
  jobName <- gsub(" ", "", substring(jobCheck, 1, 15))
  jobNum <- as.numeric(gsub(" ", "", substring(jobCheck, 22, 23)))
  checkTrue(all(jobName == c("PEND", "RUN", "DONE", "EXIT", "SUSPEND")), msg = "Check correct job names and correct order")
  checkTrue(all(jobNum == c(2, 1, 1, 1, 4)), msg = "Check correct number of jobs of each category")
  
  # Test2: check invalid values
  testVec2 <- c("PEND", "WAIT", "X", "Y", "Z", "ZOMBI")
  checkException(writeLogFile(testVec2, Sys.time(), workingPath = tempDir), msg = "Check invalid values for status input", silent = TRUE)
  checkException(writeLogFile(testVec1, 1:5, workingPath = tempDir), msg = "Check invalid input for starting time", silent = TRUE)
  checkException(writeLogFile(character(0), Sys.time(), workingPath = tempDir), msg = "Check no input for status field", silent = TRUE)
  
  # Test3: All jobs completed
  testVec3 <- rep(c("DONE", "EXIT"), each = 10)
  writeLogFile(testVec3, Sys.time(), workingPath = tempDir)
  x <- readLines(file.path(tempDir, "jobstatus.log"))
  grepProgress <- as.logical(length(grep("All jobs completed", x)))
  checkTrue(grepProgress, msg = "Check all jobs finished message")
  grepSuccess <- as.logical(length(grep("Successful runs: 10", x)))
  checkTrue(grepSuccess, msg = "Check successful jobs message")
  grepFailure <- as.logical(length(grep("Unsuccessful runs: 10", x)))
  checkTrue(grepFailure, msg = "Check unsuccessful jobs message")
  
  try(unlink(tempDir, recursive = TRUE))
}


test.writeLogFile()
