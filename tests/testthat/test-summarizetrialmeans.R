test_that("test.summarizeTrialMeans", {

  set.seed(12345)
  # Generate summary data
  myDf <- data.frame(Y1=rnorm(100), Y2=rnorm(100), Y3=rnorm(100),
                     X1=sample(1:3, 100, T), X2=sample(1:3, 100, T), X3=sample(1:3, 100, T))
  myDf$Y1[sample(1:100, 5)] <- NA
  myDf$Y2[sample(1:100, 5)] <- NA
  myDf$Y3[sample(1:100, 5)] <- NA
  quickTest <- myDf$X1 == 2 & myDf$X2 == 2 & myDf$X3 == 2
  myDf <- myDf[!quickTest,]

  # Set elements for summary
  alphaVec <- c(90, 95, 99)
  xVars <- c("X1", "X2", "X3")
  yVars <- c("Y1", "Y2", "Y3")

  # Check Exceptions
  expect_error(summarizeTrialMeans(1:5), info = "First input must be a data frame")
  expect_error(summarizeTrialMeans(myDf, letters), info = "Single response variable")
  expect_error(summarizeTrialMeans(myDf, "hello"), info = "Unknown response variable")
  expect_error(summarizeTrialMeans(myDf, "Y1", bVar = NULL), info = "No By Variables")
  expect_error(summarizeTrialMeans(myDf, "Y1", bVar = c("X1", "Hello")), info = "Some By Variables missing")
  expect_error(summarizeTrialMeans(myDf, "Y1", bVar = "X1", method = "hello"), info = "Wrong method")
  expect_error(summarizeTrialMeans(myDf, "Y1", bVar = "X1", alpha = "hello"), info = "Wrong alpha")

  # Calculate summaries using summarizeTrialMeans
  qSummaryList <- suppressWarnings(lapply(1:3,
                         function(i, df, yVars, xVars, alphaVec)
                           summarizeTrialMeans(df, yVars[i], xVars[1:i], alpha=alphaVec[i], digits=i + 1, method="Q"),
                         df=myDf, yVars = yVars, xVars = xVars, alphaVec = alphaVec))
  gSummaryList <- suppressWarnings(lapply(1:3,
                         function(i, df, yVars, xVars, alphaVec)
                           summarizeTrialMeans(df, yVars[i], xVars[1:i], alpha=alphaVec[i], digits=i + 1, method="G"),
                         df=myDf, yVars = yVars, xVars = xVars, alphaVec = alphaVec))

  # Function for recreating data explicitly
  summaryFun <- function(x, method="q", alpha=95, digits=2) {
    alpha <- checkSimAlpha(alpha)
    if (sum(!is.na(x)) == 1) {
      myLower <- myUpper <- x[!is.na(x)]
    }
    else {
      if (method == "q") {
        myLower <- quantile(x, (1-alpha)/2, na.rm=T)
        myUpper <- quantile(x, (1+alpha)/2, na.rm=T)
      }
      else {
        myLower <- mean(x, na.rm=T) - qnorm((1+alpha)/2) * sd(x, na.rm=T)
        myUpper <- mean(x, na.rm=T) + qnorm((1+alpha)/2) * sd(x, na.rm=T)
      }
    }
    round(c(Median = median(x, na.rm=T), Mean=mean(x, na.rm=T), Lower=as.vector(myLower),
            Upper=as.vector(myUpper), Min=min(x, na.rm=T), Max=max(x, na.rm=T), N=sum(!is.na(x))),
          digits)
  }

  # Compare created summaries: Quantile method, alpha = 90%, digits = 2
  qData.Y1.90 <- tapply(myDf$Y1, myDf$X1, summaryFun, method="q", alpha=90, digits=2)
  qData.Y1.90 <- matrix(unlist(qData.Y1.90), ncol=7, byrow=T)
  check1 <- as.matrix(qSummaryList[[1]][,-1])
  dimnames(check1) <- NULL
  expect_equal(check1, qData.Y1.90, info = "Quantile method, alpha 90%, digits 2")


  # Compare created summaries: Gaussian method, alpha = 90%, digits = 2
  gData.Y1.90 <- tapply(myDf$Y1, myDf$X1, summaryFun, method="g", alpha=90, digits=2)
  gData.Y1.90 <- matrix(unlist(gData.Y1.90), ncol=7, byrow=T)
  check2 <- as.matrix(gSummaryList[[1]][,-1])
  dimnames(check2) <- NULL
  expect_equal(check2, gData.Y1.90, info = "Gaussian Method, alpha = 90%, digits = 2")

  # Compare created summaries: Quantile method, alpha = 95%, digits = 3
  qData.Y2.95 <- tapply(myDf$Y2, list(myDf$X2, myDf$X1), summaryFun, method="q", alpha=95, digits=3)
  qData.Y2.95 <- matrix(unlist(qData.Y2.95), ncol=7, byrow=T)
  check3 <- as.matrix(qSummaryList[[2]][,-(1:2)])
  dimnames(check3) <- NULL
  expect_equal(check3, qData.Y2.95, info = "Quantile method, alpha = 95%, digits = 3")

  # Compare created summaries: Gaussian method, alpha = 95%, digits = 3
  gData.Y2.95 <- tapply(myDf$Y2, list(myDf$X2, myDf$X1), summaryFun, method="g", alpha=95, digits=3)
  gData.Y2.95 <- matrix(unlist(gData.Y2.95), ncol=7, byrow=T)
  check4 <- as.matrix(gSummaryList[[2]][,-(1:2)])
  dimnames(check4) <- NULL
  expect_equal(check4, gData.Y2.95, info = "Gaussian method, alpha = 95%, digits = 3")

  # Compare created summaries: Quantile method, alpha = 99%, digits = 4
  qData.Y3.99 <- tapply(myDf$Y3, list(myDf$X3, myDf$X2, myDf$X1), summaryFun, method="q", alpha=99, digits=4)
  qData.Y3.99 <- matrix(unlist(qData.Y3.99), ncol=7, byrow=T)
  check5 <- as.matrix(qSummaryList[[3]][,-(1:3)])
  dimnames(check5) <- NULL
  expect_equal(check5, qData.Y3.99, info = "Quantile method, alpha = 99%, digits = 4")

  # Compare created summaries: Gaussian method, alpha = 99%, digits = 4
  gData.Y3.99 <- tapply(myDf$Y3, list(myDf$X3, myDf$X2, myDf$X1), summaryFun, method="g", alpha=99, digits=4)
  gData.Y3.99 <- matrix(unlist(gData.Y3.99), ncol=7, byrow=T)
  check6 <- as.matrix(gSummaryList[[3]][,-(1:3)])
  dimnames(check6) <- NULL
  expect_equal(check6, gData.Y3.99, info = "Gaussian method, alpha = 99%, digits = 4")
})

