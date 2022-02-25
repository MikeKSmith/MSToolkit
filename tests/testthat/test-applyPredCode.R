test_that("test.data.applyPredCode", {
  myDf <- data.frame(X = 1:5, TH1 = rep(1, 5), TH2 = rep(2, 5), ETA1 = rep(3, 5), EPS1 = rep(4, 5))
  predCode <- c(
    "TEST = 1",
    "XCOPY = X",
    "TH2COPY = THETA(2)",
    "Y = XCOPY + LOG(THETA(1)) + THETA(2)**2 + ETA(1) + SQRT(EPS(1)) + 1")

  outDf <- applyPredCode(myDf, parsePredCode(predCode), "RESP", FALSE, c("TEST", "XCOPY", "TH2COPY", "RESP"))

  myDf$TEST <- rep(1, 5); myDf$XCOPY <- myDf$X; myDf$TH2COPY <- myDf$TH2
  myDf$RESP <- with(myDf, XCOPY + log(TH1) + TH2^2 + ETA1 + sqrt(EPS1) + 1)
  expect_true(identical(outDf, myDf))
})
