test_that("test.performAnalysis", {

  # Example data
  myDf <- data.frame(SUBJ = 1:80, .dose = rep(c(0, 15, 30, 45), each = 20))
  myDf$.interim <- rep(1:2, 40)
  myDf$RESP <- log(with(myDf, abs(exp(sqrt(.dose) + 1 + rnorm(nrow(myDf))))))

  # Analysis code
  lmCode <- function(data) {
    myLm <- lm(RESP ~ .dose, data = data)
    newData <- data.frame(.dose = sort(unique(data$.dose)))
    doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
    names(doseMeans) <- c("Mean", "Lower", "Upper")
    data.frame(.dose = sort(unique(data$.dose)), doseMeans, N = as.vector(table(data$.dose)))
  }

  # Perform analysis
  out1 <- performAnalysis(lmCode, data = myDf, doseCol = ".dose", interimCol = ".interim")

  # Tests
  expect_true(is.data.frame(out1) & all(names(out1) == c(".dose", "Mean", "Lower", "Upper", "N")))
  expect_true(all(out1$N == 20))

  # Allow analysis of a single dataset given R Script reference
  # Example data
  myDf <- data.frame(SUBJ = 1:80, DOSE = rep(c(0, 15, 30, 45), each = 20))
  myDf$INTERIM <- rep(1:2, 40)
  myDf$RESP <- log(with(myDf, abs(exp(sqrt(DOSE) + 1 + rnorm(nrow(myDf))))))

  # Perform analysis
  whichPath <- test_path("SystemTest","data","Scripts")
  out1 <- performAnalysis("rAnalysisScript.R", data = myDf, workingPath = whichPath)

  # Tests
  expect_true(is.data.frame(out1) & all(names(out1) == c("DOSE", "Mean", "Lower", "Upper", "N")))
  expect_true(all(out1$N == 20))

  # Allow subsets and data steps to be made to the data before analysing
  # Example data
  myDf <- data.frame(SUBJ = 1:80, DOSE = rep(c(0, 15, 30, 45), each = 20))
  myDf$INTERIM <- rep(1:2, 40)
  myDf$RESP <- log(with(myDf, abs(exp(sqrt(DOSE) + 1 + rnorm(nrow(myDf))))))

  # Analysis code
  lmCode <- function(data) {
    myLm <- lm(RESP ~ DOSE, data = data)
    newData <- data.frame(DOSE = sort(unique(data$DOSE)))
    doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
    names(doseMeans) <- c("Mean", "Lower", "Upper")
    data.frame(DOSE = sort(unique(data$DOSE)), doseMeans, N = as.vector(table(data$DOSE)))
  }

  # Perform analysis
  out1 <- performAnalysis(lmCode, data = myDf,
                          includeRows = cbind(c(1, 1, 1, 2, 2, 2), c(0, 15, 30, 0, 30, 45)))

  # Tests
  expect_true(is.data.frame(out1) & all(names(out1) == c("DOSE", "Mean", "Lower", "Upper", "N")))
  expect_true(all(out1$N == rep(c(20, 10), 2)))

  # Allow checking of return structure against
  # expected "Micro evaluation" return format
  myDf <- data.frame(SUBJ = 1:80, .dose = rep(c(0, 15, 30, 45), each = 20))
  myDf$.interim <- rep(1:2, 40)
  myDf$RESP <- log(with(myDf, abs(exp(sqrt(.dose) + 1 + rnorm(nrow(myDf))))))

  # Analysis code
  aCode1 <- function(data) 1:5
  aCode2 <- function(data) data.frame(X = 1:5)

  # Perform analysis
  out1 <- try(performAnalysis(aCode1, data = myDf), silent = TRUE)
  out2 <- try(performAnalysis(aCode2, data = myDf), silent = TRUE)
  expect_true(class(out1) == "try-error")
  expect_true(is.data.frame(out2))
})
