test_that("test.interimAnalysis", {

  expect_error(interimAnalysis(diag(3), "x"),
               info = "data is not a data frame")

  expect_error(interimAnalysis(mtcars, "Sepal.Length"),
               info = "code generating error" )

  expect_error(interimAnalysis(mtcars, "mpg + 2"),
               info = "code must be a function")

  expect_error(interimAnalysis(mtcars, "thatFunctionDoesNotExist"),
               info = "code must be a function that exists")

  expect_error(interimAnalysis(mtcars, function(data) list(wrongname = data$mpg + 2)),
               info = "wrong list name")

  expect_equal(list(), interimAnalysis(mtcars, function(data) list()),
               info = "empty list")

  expect_error(interimAnalysis(mtcars, function(data) list(DROP = 1, STOP = FALSE, DOSE = 3)),
               info = "too big list")

  expect_error(interimAnalysis(mtcars, function(data) list(DROPME = 1, STOPME = FALSE)),
               info = "Incorrect list element names")

  expect_error(interimAnalysis(mtcars, function(data) list(DROP = 'a', STOP = FALSE)),
               info = "DROP must be number or a logical")

  expect_error(interimAnalysis(mtcars, function(data) list(DROP = c(0,10), STOP = c(FALSE, TRUE))),
               info = "STOP must be a logical of length 1")

  expect_error(interimAnalysis(mtcars, function(data) list(DROP = c(0,10), STOP = 18)),
               info = "STOP must be a logical")

  expect_equal(list(), interimAnalysis(mtcars),
               info = "empty `interimCode`")

  expect_equal(list(), interimAnalysis(mtcars, NULL),
               info = "NULL `interimCode`")

  myData <- data.frame(DOSE=c(0, 15, 30), TEST = 1:3)

  myFun <- function(data) {
    outList <- list()
    outList$STOP <- any(data$TEST) > 5
    myTest <- data$TEST > 2
    if (any(myTest)) outList$DROP <- data$DOSE[myTest]
    outList
  }

  expect_equal(list(STOP = FALSE, DROP = 30), interimAnalysis(myData, myFun))

  myData <- data.frame(DOSE=c(0, 15, 30), TEST = 1:3)

  myFun <- function(data) {
    outList <- list()
    outList$STOP <- any(data$TEST) > 5
    myTest <- data$TEST > 2
    if (any(myTest)) outList$KEEP <- myTest
    outList
  }

  expect_equal(list(STOP = FALSE, KEEP = c(F, F, T)), interimAnalysis(myData, myFun))
})
