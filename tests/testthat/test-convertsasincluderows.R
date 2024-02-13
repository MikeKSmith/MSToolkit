test_that("test.convertSASIncludeRows", {

  expect_true(convertSASIncludeRows() == "**** No doses to drop ****;")
  expect_error(convertSASIncludeRows(1:5))
  expect_error(convertSASIncludeRows(rbind(1:5)))

  inMat <- cbind(1:3, c(0, 15, 30))
  str1 <- convertSASIncludeRows(inMat, doseCol = "DOSECOL", interimCol = "INTERIMCOL")
  target1 <- "IF (INTERIMCOL = 1 and DOSECOL = 0) or (INTERIMCOL = 2 and DOSECOL = 15) or (INTERIMCOL = 3 and DOSECOL = 30);"
  expect_true(str1 == target1)
})
