library(MSToolkit)

#---- meta.columnNames

test_that("test.meta.columnNames", {
  resetEctdColNames()
  expect_error(getEctdColName(),
               info = "Invalid input to .getEctdColName : Missing")
  expect_error(getEctdColName(1),
               info = "Invalid input to .getEctdColName : Numeric")
  expect_error(getEctdColName(letters),
               info = "Invalid input to .getEctdColName : Multiple")
  expect_error(getEctdColName("XXX"),
               info = "Invalid column name .getEctdColName")
  expect_true(getEctdColName("Time") == "TIME",
              info = "Check correct Time column")
  expect_true(getEctdColName("Dose") == "DOSE",
              info = "Check correct Dose column")
  expect_true(getEctdColName("Subject") == "SUBJ",
              info = "Check correct Subject column")
  expect_true(getEctdColName("Interim") == "INTERIM",
              info = "Check correct Interim column")
  expect_true(getEctdColName("ParOmit") == "PAROMIT",
              info = "Check correct ParOmit column")
  expect_true(getEctdColName("RespOmit") == "RESPOMIT",
              info = "Check correct RespOmit column")
  expect_true(getEctdColName("Response") == "RESP",
              info = "Check correct Response column")
  expect_true(getEctdColName("Trt") == "TRT",
              info = "Check correct Treatment column")
  expect_true(getEctdColName("Missing") == "MISSING",
              info = "Check correct Missing column")
  expect_true(getEctdColName("Replicate") == "Replicate",
              info = "Check correct Replicate column")
  expect_error(setEctdColName(),
               info = "Invalid input to .setEctdColName: Missing colName")
  expect_error(setEctdColName(1),
               info = "Invalid input to .setEctdColName: Numeric colName")
  expect_error(setEctdColName(letters),
               info = "Invalid input to .setEctdColName: Multiple colName")
  expect_error(setEctdColName("X"),
               info = "Invalid input to .setEctdColName: Missing Value")
  expect_error(setEctdColName("X", 1),
               info = "Invalid input to .setEctdColName: Numeric Value")
  expect_error(setEctdColName("X", letters),
               info = "Invalid input to .setEctdColName: Multiple Value")
  expect_error(setEctdColName("X", "TEST"),
               info = "Invalid input to .setEctdColName: Non-matching Column Name")
  setEctdColName("Dose", "TEST")
  expect_equal("TEST", getEctdColName("Dose"),
               info = "Sets a default column name correctly")
  resetEctdColNames()
  expect_true(getEctdColName("Time") == "TIME",
              info = "Check correct Time column")
  expect_true(getEctdColName("Dose") == "DOSE",
              info = "Check correct Dose column")
  expect_true(getEctdColName("Subject") == "SUBJ",
              info = "Check correct Subject column")
  expect_true(getEctdColName("Interim") == "INTERIM",
              info = "Check correct Interim column")
  expect_true(getEctdColName("ParOmit") == "PAROMIT",
              info = "Check correct ParOmit column")
  expect_true(getEctdColName("RespOmit") == "RESPOMIT",
              info = "Check correct RespOmit column")
  expect_true(getEctdColName("Response") == "RESP",
              info = "Check correct Response column")
  expect_true(getEctdColName("Trt") == "TRT",
              info = "Check correct Treatment column")
  expect_true(getEctdColName("Missing") == "MISSING",
              info = "Check correct Missing column")
  expect_true(getEctdColName("Replicate") == "Replicate",
              info = "Check correct Replicate column")
  expect_equal(get("colNames", env = .ectdEnv)$Time$Default,
               getEctdColName("Time"),
               info = "Check default Time column")
  expect_equal(get("colNames", env = .ectdEnv)$Dose$Default,
               getEctdColName("Dose"),
               info = "Check default Dose column")
  expect_equal(get("colNames", env = .ectdEnv)$Subject$Default,
               getEctdColName("Subject"),
               info = "Check default Subject column")
  expect_equal(get("colNames", env = .ectdEnv)$Interim$Default,
               getEctdColName("Interim"),
               info = "Check default Interim column")
  expect_equal(get("colNames", env = .ectdEnv)$ParOmit$Default,
               getEctdColName("ParOmit"),
               info = "Check default ParOmit column")
  expect_equal(
    get("colNames", env = .ectdEnv)$RespOmit$Default,
    getEctdColName("RespOmit"),
    info = "Check default RespOmit column"
  )
  expect_equal(
    get("colNames", env = .ectdEnv)$Response$Default,
    getEctdColName("Response"),
    info = "Check default Response column"
  )
  expect_equal(get("colNames", env = .ectdEnv)$Trt$Default,
               getEctdColName("Trt"),
               info = "Check default Trt column")
  expect_equal(get("colNames", env = .ectdEnv)$Missing$Default,
               getEctdColName("Missing"),
               info = "Check default Missing column")
  expect_equal(
    get("colNames", env = .ectdEnv)$Replicate$Default,
    getEctdColName("Replicate"),
    info = "Check default Replicate column"
  )
  expect_error(getEctdPossibleColNames(),
               info = "Invalid input to .getEctdPossibleColNames: Missing")
  expect_error(getEctdPossibleColNames(1),
               info = "Invalid input to .getEctdPossibleColNames: Numeric")
  expect_error(getEctdPossibleColNames(letters),
               info = "Invalid input to .getEctdPossibleColNames: Multiple")
  expect_error(getEctdPossibleColNames("XXX"),
               info = "Invalid column name .getEctdPossibleColNames")
  expect_true(getEctdColName("Time") %in% getEctdPossibleColNames("Time"),
              info = "Initial contained in possible Time values")
  expect_true(getEctdColName("Dose") %in% getEctdPossibleColNames("Dose"),
              info = "Initial contained in possible Dose values")
  expect_true(getEctdColName("Subject") %in% getEctdPossibleColNames("Subject"),
              info = "Initial contained in possible Subject values")
  expect_true(getEctdColName("Interim") %in% getEctdPossibleColNames("Interim"),
              info = "Initial contained in possible Interim values")
  expect_true(getEctdColName("ParOmit") %in% getEctdPossibleColNames("ParOmit"),
              info = "Initial contained in possible ParOmit values")
  expect_true(getEctdColName("RespOmit") %in% getEctdPossibleColNames("RespOmit"),
              info = "Initial contained in possible RespOmit values")
  expect_true(getEctdColName("Response") %in% getEctdPossibleColNames("Response"),
              info = "Initial contained in possible Response values")
  expect_true(getEctdColName("Trt") %in% getEctdPossibleColNames("Trt"),
              info = "Initial contained in possible Treatment values")
  expect_true(getEctdColName("Missing") %in% getEctdPossibleColNames("Missing"),
              info = "Initial contained in possible Missing values")
  expect_true(getEctdColName("Replicate") %in% getEctdPossibleColNames("Replicate"),
              info = "Initial contained in possible Replicate values")
  expect_true(all("ID" %in% getEctdPossibleColNames("Subject")),
              info = "Extra possible Subject values")
  expect_true(all(c("DAY", "WEEK") %in% getEctdPossibleColNames("Time")),
              info = "Extra possible Time values")
  expect_true(all("DV" %in% getEctdPossibleColNames("Response")),
              info = "Extra possible Response values")
  expect_true(all("TRIAL" %in% getEctdPossibleColNames("Replicate")),
              info = "Extra possible Replicate values")
  expect_error(getEctdPossibleColNames("X"))
  expect_error(getEctdPossibleColNames(c("Subject", "Dose")))
  expect_error(setEctdPossibleColNames("X", LETTERS))
  expect_error(setEctdPossibleColNames(c("Subject", "Dose"),
                                       LETTERS))
  expect_error(setEctdPossibleColNames("Subject", 1:10))
  resetEctdColNames()
  origPoss <- c("SUBJ", "ID")
  expect_true(all(getEctdPossibleColNames("Subject") == origPoss))
  setEctdPossibleColNames("Subject", LETTERS)
  expect_true(all(getEctdPossibleColNames("Subject") == c("SUBJ",
                                                          LETTERS)))
  setEctdPossibleColNames("Subject", origPoss)
  expect_true(all(getEctdPossibleColNames("Subject") == origPoss))
  resetEctdColNames()
  setEctdPossibleColNames("Subject", origPoss)
  expect_true(length(matchEctdColNames("Subject", LETTERS))==0)
  expect_true(matchEctdColNames("Subject", c(letters, "SUBJ")) ==
                "SUBJ")
})

#---- meta.dataStorageMethod

test_that("test.meta.dataStorageMethod", {
  currentDataMethod <- getEctdDataMethod()
  expect_equal(get("dataStoreMethod", env = .ectdEnv),
               currentDataMethod,
               info = "Extracts data method correctly")
  expect_error(setEctdDataMethod(),
               info = "No input to .setEctdDataMethod")
  expect_error(setEctdDataMethod("X"),
               info = "Invalid method specified in .setEctdDataMethod")
  setEctdDataMethod("R")
  expect_equal("RData", getEctdDataMethod(),
               info = "Sets the data method correctly: RData")
  setEctdDataMethod("I")
  expect_equal("Internal", getEctdDataMethod(),
               info = "Sets the data method correctly: Internal")
  setEctdDataMethod("C")
  expect_equal("CSV", getEctdDataMethod(),
               info = "Sets the data method correctly: CSV")
  setEctdDataMethod(currentDataMethod)
})

#---- meta.dateFormat

test_that("test.meta.dateFormat", {
  currentDateFormat <- getEctdDateFormat()
  expect_equal(get("dateFormat", env = .ectdEnv), currentDateFormat,
               info = "Extracts date format correctly")
  expect_error(setEctdDateFormat(),
               info = "No input to .setEctdDateFormat")
  setEctdDateFormat("TEST")
  expect_equal("TEST", getEctdDateFormat(),
               info = "Sets the date format correctly")
  setEctdDateFormat(currentDateFormat)
})

#---- meta.externalPath

# test_that("test.meta.externalPath", {
#   expect_error(getEctdExternalPath("XXX"),
#                info = "Invalid path for .getEctdExternalPath")
#   expect_error(getEctdExternalPath(1),
#                info = "Invalid input to .getEctdExternalPath: Numeric")
#   expect_error(getEctdExternalPath(letters),
#                info = "Invalid input to .getEctdExternalPath: Multiple")
#   # currentPath <- getEctdExternalPath("SASPATH_UNIX")
#   # expect_equal(get("externalPaths", env = .ectdEnv)["SASPATH_UNIX"],
#   #              currentPath, info = "Extracts path correctly")
#   expect_error(setEctdExternalPath(),
#                info = "Invalid input to .setEctdExternalPath: Missing pathName")
#   expect_error(setEctdExternalPath(1),
#                info = "Invalid input to .setEctdExternalPath: Numeric pathName")
#   expect_error(setEctdExternalPath(letters),
#                info = "Invalid input to .setEctdExternalPath: Multiple pathName")
#   expect_error(setEctdExternalPath("X"),
#                info = "Invalid input to .setEctdExternalPath: Missing Value")
#   expect_error(setEctdExternalPath("X", 1),
#                info = "Invalid input to .setEctdExternalPath: Numeric Value")
#   expect_error(setEctdExternalPath("X", letters),
#                info = "Invalid input to .setEctdExternalPath: Multiple Value")
#   setEctdExternalPath("SASPATH_UNIX", "TEST")
#   expect_equal(c(SASPATH_UNIX = "TEST"),
#                getEctdExternalPath("SASPATH_UNIX"),
#                info = "Sets an external path correctly")
#   setEctdExternalPath("SASPATH_UNIX", currentPath)
#   expect_true(all(names(get(
#     "externalPaths", env = .ectdEnv
#   )) ==
#     getEctdExternalPath()))
# })

#---- meta.logFile

test_that("test.meta.logFile", {
  currentLogFile <- getEctdLogFile()
  expect_equal(get("logfile", env = .ectdEnv), currentLogFile,
               info = "Extracts log file correctly")
  expect_error(setEctdLogFile(),
               info = "No input to .setEctdLogFile")
  setEctdLogFile("TEST")
  expect_equal("TEST", getEctdLogFile(),
               info = "Sets the log file correctly")
  setEctdLogFile(currentLogFile)
})

#---- meta.verbose

test_that("test.meta.verbose", {
  currentVerbose <- getEctdVerbose()
  expect_equal(get("verbose", env = .ectdEnv), currentVerbose,
               info = "Extracts verbose flag correctly")
  expect_error(setEctdVerbose(),
               info = "No input to .setEctdVerbose")
  setEctdVerbose("TEST")
  expect_equal("TEST", getEctdVerbose(),
               info = "Sets the verbose flag correctly")
  setEctdVerbose(currentVerbose)
})
