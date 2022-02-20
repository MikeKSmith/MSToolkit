test_that("test.calculateSimTrialMeans", {

  # Example data
  simData <- expand.grid(TRIAL = 1:2, DOSE=c(0, 5, 15), GEN = 0:1, AGE = c(18, 35, 45, 60))
  N <- nrow(simData); simData$ID <- 1:N
  simData <- merge(simData, expand.grid(ID = 1:N, TIME = 1:3))
  simData$RESP <- rnorm(nrow(simData))

  # Check exceptions
  expect_error(calculateSimTrialMeans(simData, respType = "a", respCol = "AGE", idCol = "ID", timeCol = "TIME", doseCol = "DOSE", replicateCol = "TRIAL"))
  expect_error(calculateSimTrialMeans(simData, catType = "a", respCol = "AGE", idCol = "ID", timeCol = "TIME", doseCol = "DOSE", replicateCol = "TRIAL"))
  expect_error(calculateSimTrialMeans(simData, diffBase = 1, respCol = "AGE", idCol = "ID", timeCol = "TIME", doseCol = "DOSE", replicateCol = "TRIAL"))
  expect_error(calculateSimTrialMeans(simData, fillRespRange = 1, respCol = "AGE", idCol = "ID", timeCol = "TIME", doseCol = "DOSE", replicateCol = "TRIAL"))
  expect_error(calculateSimTrialMeans(simData, respCol = 1, idCol = "ID", timeCol = "TIME", doseCol = "DOSE", replicateCol = "TRIAL"))
  expect_error(calculateSimTrialMeans(simData, respCol = "RESP", idCol = letters, doseCol = "DOSE", replicateCol = "TRIAL"))
  expect_error(calculateSimTrialMeans(simData, respCol = "RESP", idCol = "ID", doseCol = TRUE, replicateCol = "TRIAL"))
  expect_error(calculateSimTrialMeans(simData, respCol = "RESP", idCol = "ID", doseCol = "DOSE", replicateCol = NULL))
  expect_error(calculateSimTrialMeans(simData, respCol = "RESP", idCol = "ID", doseCol = "DOSE", replicateCol = "TRIAL", digits = "hello"))
  expect_error(calculateSimTrialMeans(1:5, respCol = "RESP", idCol = "ID", doseCol = "DOSE", replicateCol = "TRIAL"))
  expect_error(calculateSimTrialMeans(data.frame(X = 1:5), respCol = "RESP", idCol = "ID", doseCol = "DOSE", replicateCol = "TRIAL"))

  # Check calculation is correct
  simData <- expand.grid(TRIAL = 1:2, DOSE=c(0, 5, 15), GEN = 0:1, AGE = c(18, 35, 45, 60))
  N <- nrow(simData); simData$ID <- 1:N
  simData <- merge(simData, expand.grid(ID = 1:N, TIME = 1:3))
  simData$RESP <- rnorm(nrow(simData))
  out1 <- calculateSimTrialMeans(simData, respCol = "AGE", replicateCol = "TRIAL",
                                 doseCol = "DOSE", timeCol = "TIME", idCol = "ID")
  expect_true(all(dim(out1) == c(6, 3)) && all(names(out1) == c("TRIAL", "DOSE", "AGE")))
  expect_true(all(out1$AGE == 39.5))

  # Test naming of variables
  simData <- expand.grid(.TRIAL = 1:2, .ID = 1, .TIME = 1:2, .DOSE=c(5, 15), .GEN = 0:1)
  simData$.RESP <- rnorm(nrow(simData))
  out <- calculateSimTrialMeans(simData, respCol = ".RESP", replicateCol = ".TRIAL",
                                idCol = ".ID", bVar = ".GEN", doseCol = ".DOSE", timeCol = ".TIME")
  expect_true(all(names(out) == c(".TRIAL", ".GEN", ".RESP")))

  # Test random number generation
  simData <- expand.grid(TRIAL = 1:2, DOSE=c(0, 5, 15), GEN = 0:1, AGE = c(18, 35, 45, 60))
  N <- nrow(simData); simData$ID <- 1:N
  simData <- merge(simData, expand.grid(ID = 1:N, TIME = 1:3))
  simData$RESP <- rnorm(nrow(simData))
  out1 <- calculateSimTrialMeans(simData, respCol = "RESP", replicateCol = "TRIAL",
                                 idCol = "ID", bVar = "GEN", doseCol = "DOSE", timeCol = "TIME", digits = 10)
  byHand <- aggregate(list(RESP = simData$RESP), simData[c("TRIAL", "ID", "DOSE", "TIME", "GEN")], mean)
  byHand <- aggregate(list(RESP = byHand$RESP), byHand[c("TRIAL", "GEN")], mean)
  expect_true(all(round(out1$RESP, 4) == round(byHand$RESP, 4)))

  # Check rounding
  simData <- expand.grid(TRIAL = 1:2, DOSE=c(0, 5, 15), GEN = 0:1, AGE = c(18, 35, 45, 60))
  N <- nrow(simData); simData$ID <- 1:N
  simData <- merge(simData, expand.grid(ID = 1:N, TIME = 1:3))
  simData$RESP <- rnorm(nrow(simData))
  out1 <- calculateSimTrialMeans(simData, respCol = "RESP", replicateCol = "TRIAL",
                                 idCol = "ID", bVar = "GEN", doseCol = "DOSE", timeCol = "TIME", digits = 10)
  out2 <- calculateSimTrialMeans(simData, respCol = "RESP", replicateCol = "TRIAL",
                                 idCol = "ID", bVar = "GEN", doseCol = "DOSE", timeCol = "TIME", digits = 4)
  out3 <- calculateSimTrialMeans(simData, respCol = "RESP", replicateCol = "TRIAL",
                                 idCol = "ID", bVar = "GEN", doseCol = "DOSE", timeCol = "TIME", digits = 2)
  test1 <- all(out1$RESP != out2$RESP) & all(round(out1$RESP, 4) == out2$RESP)
  test2 <- all(out1$RESP != out3$RESP) & all(round(out1$RESP, 2) == out3$RESP)
  expect_true(test1 & test2)

  # Check subsetting + diffBase
  simData <- expand.grid(TRIAL = 1:2, DOSE=c(0, 5, 15), GEN = 0:1, AGE = c(18, 35, 45, 60))
  N <- nrow(simData); simData$ID <- 1:N
  simData <- merge(simData, expand.grid(ID = 1:N, TIME = -1:3))
  simData$RESP <- rnorm(nrow(simData))
  simData$RESP [simData$TIME <= 0] <- 1
  out1 <- calculateSimTrialMeans(simData, respCol = "RESP", replicateCol = "TRIAL", idCol = "ID", bVar = "GEN",
                                 doseCol = "DOSE", timeCol = "TIME", diffBase= FALSE, subset = "TIME > 0")$RESP
  out2 <- calculateSimTrialMeans(simData, respCol = "RESP", replicateCol = "TRIAL",idCol = "ID", bVar = "GEN",
                                 doseCol = "DOSE", timeCol = "TIME", diffBase= TRUE)$RESP
  out3 <- calculateSimTrialMeans(simData, respCol = "RESP", replicateCol = "TRIAL",idCol = "ID", bVar = "GEN",
                                 doseCol = "DOSE", timeCol = "TIME", subset = "AGE > 20" )$RESP
  out4 <- calculateSimTrialMeans(subset(simData, AGE > 20), respCol = "RESP", replicateCol = "TRIAL",idCol = "ID",
                                 bVar = "GEN", doseCol = "DOSE", timeCol = "TIME" )$RESP
  expect_true(all(round(out1 - out2 - 1, 2) == 0))
  expect_true(all(out3 == out4))

  # Handling of missing categories for summarisation i.e. aggregate-like or tapply-like?
  simData <- expand.grid(TRIAL = 1:2, DOSE=c(0, 5, 15), GEN = 0:1, AGE = c(18, 35, 45, 60))
  N <- nrow(simData); simData$ID <- 1:N
  simData <- merge(simData, expand.grid(ID = 1:N, TIME = -1:3))
  simData$RESP <- rnorm(nrow(simData))
  whichRows <- with(simData, DOSE == 0 & GEN == 0)
  simData <- simData[!whichRows, ]
  out <- calculateSimTrialMeans(simData, respCol = "RESP", replicateCol = "TRIAL",
                                idCol = "ID", bVar = c("GEN", "DOSE"), doseCol = "DOSE", timeCol = "TIME")
  expect_true(nrow(out) == 10)
  expect_true( !any(is.na(out$RESP)))
})
