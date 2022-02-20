test_that("test.diffsFromBaseline", {

  # Generate some test data
  myDf <- expand.grid(xReplicate = 1:2, xSUBJ = 1:5, xTIME = -1:2)
  myDf$xRESP <- myDf$xTIME + 2
  myDf <- myDf [do.call("order", myDf[c("xReplicate", "xSUBJ", "xTIME")]), ]
  oneSubjectNot <- with(myDf, xReplicate == 1 & xSUBJ == 1 & xTIME <= 0)
  setEctdColName("Subject", "xSUBJ")
  setEctdColName("Replicate", "xReplicate")
  setEctdColName("Time", "xTIME")
  setEctdColName("Response", "xRESP")

  # Calling Exceptions
  nowWarn <- options()$warn
  options(warn = 2)
  expect_error(calculateDiffsFromBase(1:5), info = "Calling with incorrect first input")
  expect_error(calculateDiffsFromBase(myDf, idCol = "X"), info = "Calling with incorrect subject variable name")
  expect_error(calculateDiffsFromBase(myDf, timeCol = "X"), info = "Calling with incorrect time variable name")
  expect_error(calculateDiffsFromBase(myDf, respCol = "X"), info = "Calling with incorrect response variable name")
  expect_error(calculateDiffsFromBase(myDf, repColCol = "X"), info = "Calling with incorrect response variable name")
  expect_error(calculateDiffsFromBase(myDf, baseDef = "..XX"), info = "Incorrect input for baseline definition")
  expect_error(calculateDiffsFromBase(myDf, baseDef = "xTIME < -5"), info = "No baseline data")
  expect_error(calculateDiffsFromBase(myDf, baseDef = "xTIME < 5"), info = "All baseline data")
  expect_error(calculateDiffsFromBase(myDf[!oneSubjectNot,]), info = "One subject without baseline")
  options(warn = nowWarn)

  # These should work fine
  out1 <- calculateDiffsFromBase(myDf)
  out2 <- calculateDiffsFromBase(myDf, tolerance = 1.6)
  out3 <- calculateDiffsFromBase(myDf, removeBaseline = FALSE)
  sub1 <- out1 [out1$xReplicate == 1 & out1$xSUBJ == 1, ]$xRESP
  sub2 <- out2 [out2$xReplicate == 1 & out2$xSUBJ == 1, ]$xRESP
  sub3 <- out3 [out3$xReplicate == 1 & out3$xSUBJ == 1, ]$xRESP
  expect_true(length(sub1) == 2 && all(sub1 == c(1.5, 2.5)), info = "Correct call")
  expect_true(length(sub2) == 2 && all(sub2 == c(0, 2.5)), info = "Correct call with tolerance")
  expect_true(length(sub3) == 4 && all(sub3 == c(-.5, .5, 1.5, 2.5)), info = "Correct call keeping baseline")

  # Reset to default column names
  resetEctdColNames()
})
