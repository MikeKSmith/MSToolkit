if( !exists("unitTestPath")) unitTestPath <- system.file(package = "MSToolkit", "tests")
macro.datapath <- file.path(unitTestPath , "testthat", "data", "macroEvaluation")

test_that("test.analysis.macro", {

  expect_error(macroEvaluation(doseCol = "DOSE, DOSES"),
               info = "dose too long")
  expect_error(macroEvaluation(doseCol = "01erwfwjui3w4"),
               info = "dose not valid")
  expect_error(macroEvaluation(interimCol = "INTSA, INTERIM"),
               info = "interim too long")
  expect_error(macroEvaluation(interimCol = "=08fewik3"),
               info = "interim not valid")

  microData <- read.csv(file.path(macro.datapath, "micro0001.csv" ), header = TRUE )
  expect_error(macroEvaluation(doseCol = "D", data = microData),
               info = "dose not in the data")
  expect_error(macroEvaluation(interimCol = "I", data = microData),
               info = "interim not in the data")

  wrongCode <- function(data) stop("error")
  expect_error(macroEvaluation(data = microData, macroCode = wrongCode),
               info = "wrong code, generates error")

  #  expect_error(macroEvaluation(data = microData, macroCode = "ghost"),
  #               info = "code does not exist")

  expect_error(macroEvaluation(data = microData, macroCode = ghost),
               info = "code does not exist")

  mCode1 <- function(data) {
    diffMeans <- data$MEAN[data$DOSE == 100] - data$MEAN[data$DOSE == 0]
    data.frame(SUCCESS = diffMeans > 10, NFINAL = sum(data$N))
  }
  expect_error(macroEvaluation(data = microData, macroCode = mCode1),
               info = "code generates data with more than one line")

  mCode2 <- function(data) {
    diffMeans <- data$MEAN[data$DOSE == 100 & data$INTERIM == 0] -
      data$MEAN[data$DOSE == 0 & data$INTERIM == 0]
    data.frame(SUCCESS = diffMeans > 10, NFINAL = sum(data$N))
  }
  out <- macroEvaluation(data = microData, macroCode = mCode2)
  expect_true(all(dim(out) == c(1,2)),
              info = "cheking with correct code")
})
