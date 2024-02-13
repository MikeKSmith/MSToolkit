test_that("test.calculateObsSummary", {

  # Create test (xpd) objects
  myDf1 <- data.frame(ID = rep(1:6, each = 10), TIME = rep(-1:8, 6),
                      DOSE = rep( rep(c(0, 15, 30), 2), each = 10))
  myDf1$DV <- myDf1$ID + myDf1$DOSE
  myDf2 <- data.frame(SUBJ = rep(1:6, each = 10), DAY = rep(-1:8, 6),
                      DOSE = rep( rep(c(0, 15, 30), 2), each = 10))
  myDf2$DV <- myDf2$SUBJ + myDf2$DAY

  # Exceptions
  expect_error(calculateObsSummary(), info = "No arguments")
  expect_error(calculateObsSummary(respType = "X"), info = "Response Type")
  expect_error(calculateObsSummary(catType = "X"), info = "Response Type")
  expect_error(calculateObsSummary(diffBase = 1), info = "DiffBase Flag 1")
  expect_error(calculateObsSummary(fillRespRange = "Hello"), info = "DiffBase Flag 1")
  expect_error(calculateObsSummary(diffBase = rep(T, 2)), info = "DiffBase Flag 2")
  expect_error(calculateObsSummary(fillRespRange = rep(T, 2)), info = "DiffBase Flag 2")
  expect_error(calculateObsSummary(respCol = 1), info = "Resp col 2")
  expect_error(calculateObsSummary(respCol = letters), info = "Resp col 2")
  expect_error(calculateObsSummary(idCol = 1), info = "Subject col 2")
  expect_error(calculateObsSummary(idCol = letters), info = "Subject col 2")
  expect_error(calculateObsSummary(digits = "A"), info = "Digits 1")
  expect_error(calculateObsSummary(digits = 1:4), info = "Digits 2")
  expect_error(calculateObsSummary(alpha = "Hello"), info = "alpha")
  expect_error(calculateObsSummary(myDf1, bVar = NULL), info = "By variables")
  expect_error(calculateObsSummary(myDf1, respCol = "NOTINTHERE"), info = "Response Column")
  expect_error(calculateObsSummary(myDf1, respCol = "DV", bVar = "NOTINTHERE"),
               info = "Trellis Column")
  expect_error(calculateObsSummary(myDf1, respCol = "DV", bVar = "DOSE", idCol = "NOTINTHERE"),
               info = "Subject Column")

  # Create test summaries
  os1 <- calculateObsSummary(myDf1, "DV", bVar = "DOSE", subset = "ID < 5", alpha = .9, idCol = "ID", timeCol="TIME")
  os2 <- calculateObsSummary(myDf1, "DV", bVar = "ID", idCol = "ID", timeCol="TIME")
  os3 <- calculateObsSummary(myDf1, "DV", bVar = "TIME", subset = "ID < 5", digits = 2, idCol = "ID", timeCol="TIME")
  os4 <- calculateObsSummary(myDf1, "DV", bVar = "ID", diffBase = T, idCol = "ID", timeCol="TIME")
  os5 <- calculateObsSummary(myDf2, "DV", bVar = "DOSE", diffBase = T, idCol = "SUBJ", timeCol="DAY")
  expect_error(calculateObsSummary(myDf1))

  # Test: os1
  expect_true(all(os1$DOSE == c(0, 15, 30)))
  expect_true(all(os1$Mean == c(2.5, 17, 33)))
  expect_true(all(os1$Median == c(2.5, 17, 33)))
  expect_true(all(round(os1$Lower, 3) == c(1.934, 17, 33)))
  expect_true(all(round(os1$Upper, 3) == c(3.066, 17, 33)))

  # Test: os2
  testData <- unique(myDf1[,c("ID", "DV")])
  expect_true(all(os2$N == 10))
  expect_true(all(os2$Mean == testData$DV))
  expect_true(all(os2$Lower == testData$DV))
  expect_true(all(os2$Upper == testData$DV))
  expect_true(all(os2$Min == testData$DV))
  expect_true(all(os2$Min  == testData$DV))

  # Test: os3
  expect_true(all(os3$TIME == -1:8))
  expect_true(nrow(unique(os3[-1])) == 1)
  expect_true(all(unlist(os3[1,-1]) == c(10.5, 13.75, -.55, 28.05, 1, 33, 4)))

  # Test: os4
  expect_true(all(os4$ID == 1:6))
  expect_true(nrow(unique(os4[-1])) == 1)
  expect_true(all(unlist(os4[1,-c(1, 8)]) == 0))

  # Test: os5
  expect_true(all(os5$DOSE == c(0, 15, 30)))
  expect_true(nrow(unique(os5[-1])) == 1)
  expect_true(all(unlist(os5[1,-1]) == c(5, 5, 3.84, 6.16, 1.5, 8.5, 16)))
})
