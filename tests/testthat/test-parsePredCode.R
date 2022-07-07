test_that("test.data.parsePred", {

  inTest <- c(
    "X1 = THETA(1)^EPS(2)",
    "X2 = A.GT.0.AND.B.EQ.2.OR.C.LE.5")
  outTest <- c(
    "X1  <-  TH1^EPS2",
    "X2  <-  (A > 0) & (B == 2) | (C <= 5)")
  expect_true(length(parsePredCode(inTest)) == length(outTest))
  expect_true(all(parsePredCode(inTest) == outTest))

  # non-integer values
  inTest <- c(
    "X1 = THETA(1)^EPS(2)",
    "X2 = A.GT.0.99.AND.B.EQ.2.0.OR.C.LE..51")
  outTest <- c(
    "X1  <-  TH1^EPS2",
    "X2  <-  (A > 0.99) & (B == 2.0) | (C <= .51)")
  parsePredCode(inTest)
  expect_true(length(parsePredCode(inTest)) == length(outTest))
  expect_true(all(parsePredCode(inTest) == outTest))

  inTest <- c(
    "X1=EXP(THETA(1))",
    "X2=LOG(THETA(2))",
    "X3=ABS(ETA(1))**2",
    "Y=SQRT(ETA(2)) * SIN(EPA(1))")
  outTest <- c(
    "X1 <- exp( TH1)",
    "X2 <- log( TH2)",
    "X3 <- abs( ETA1) ^ 2",
    "Y <- sqrt( ETA2)  *  sin( EPA(1))",
    "RESP <- Y  # Additional Command Added")
  expect_true(all(parsePredCode(inTest) == outTest))


  # If Else test
  ifElseText <- c(
    " IF (AGE.EQ.0) THEN",
    "  AGE1=38",
    "  AGE2=39",
    " ELSE ",
    "  AGE1=AGE",
    "  AGE2=AGE + 1",
    " ENDIF",
    " IF (CRCL.EQ.0) THEN ",
    " CRC1=106",
    "  ELSE ",
    "  CRC1=CRCL",
    "  ENDIF",
    " IF (AGE.EQ.0) STUF = 1",
    " IF (   AGE.GT.1) STUF = 2",
    "    IF (X.EQ.0) STUF = 3",
    " IF (AGE.LT.0) STUF = 4",
    " IF (X.NE.1) STUF = 5",
    " FUD=1",
    " IF(STUD.EQ.21) THEN             ",
    "FUD=THETA (6)",
    "  ELSEIF (STUD.EQ.25) THEN ",
    "    FUD=THETA(7)",
    "     ELSEIF (STUD.EQ.26) THEN",
    "      FUD=THETA(8)",
    "       ELSEIF (STUD.EQ.83) ",
    "       THEN",
    "        FUD=THETA(9)",
    "         ELSEIF(STUD.EQ.85) THEN",
    "           FUD=THETA(10)",
    "         ELSE",
    "           FUD = THETA(11)",
    "            ENDIF",
    " IF(STUD.EQ.21) THEN             ",
    "FUD=THETA (6)",
    "FUD=THETA (6)",
    "  ELSEIF (STUD.EQ.25) THEN ",
    "    FUD=THETA(7)",
    "    FUD=THETA(7)",
    "     ELSEIF (STUD.EQ.26) THEN",
    "      FUD=THETA(8)",
    "       ELSEIF (STUD.EQ.83) ",
    "       THEN",
    "        FUD=THETA(9)",
    "        FUD=THETA(9)",
    "         ELSEIF(STUD.EQ.85) THEN",
    "           FUD=THETA(10)",
    "         ELSE",
    "           TEST = 1",
    "           FUD = THETA(11)",
    "            ENDIF"
  )
  ifElseTarget <- c(
    "AGE1 [ (AGE == 0) ] <- 38",
    "AGE2 [ (AGE == 0) ] <- 39",
    "AGE1 [ !(AGE == 0) ] <- ( AGE ) [ !(AGE == 0) ]",
    "AGE2 [ !(AGE == 0) ] <- ( AGE  +  1 ) [ !(AGE == 0) ]",
    "CRC1 [ (CRCL == 0) ] <- 106",
    "CRC1 [ !(CRCL == 0) ] <- ( CRCL ) [ !(CRCL == 0) ]",
    "STUF [ (AGE == 0) ] <- 1",
    "STUF [ (AGE > 1) ] <- 2",
    "STUF [ (X == 0) ] <- 3",
    "STUF [ (AGE < 0) ] <- 4",
    "STUF [ (X != 1) ] <- 5",
    "FUD <- 1",
    "FUD [ (STUD == 21) ] <- ( TH6 ) [ (STUD == 21) ]",
    "FUD [ !(STUD == 21) & (STUD == 25) ] <- ( TH7 ) [ !(STUD == 21) & (STUD == 25) ]",
    "FUD [ !(STUD == 21) & !(STUD == 25) & (STUD == 26) ] <- ( TH8 ) [ !(STUD == 21) & !(STUD == 25) & (STUD == 26) ]",
    "FUD [ !(STUD == 21) & !(STUD == 25) & !(STUD == 26) & (STUD == 83) ] <- ( TH9 ) [ !(STUD == 21) & !(STUD == 25) & !(STUD == 26) & (STUD == 83) ]",
    "FUD [ !(STUD == 21) & !(STUD == 25) & !(STUD == 26) & !(STUD == 83) & (STUD == 85) ] <- ( TH10 ) [ !(STUD == 21) & !(STUD == 25) & !(STUD == 26) & !(STUD == 83) & (STUD == 85) ]",
    "FUD [ !(STUD == 21) & !(STUD == 25) & !(STUD == 26) & !(STUD == 83) & !(STUD == 85) ] <- ( TH11 ) [ !(STUD == 21) & !(STUD == 25) & !(STUD == 26) & !(STUD == 83) & !(STUD == 85) ]",
    "FUD [ (STUD == 21) ] <- ( TH6 ) [ (STUD == 21) ]",
    "FUD [ (STUD == 21) ] <- ( TH6 ) [ (STUD == 21) ]",
    "FUD [ !(STUD == 21) & (STUD == 25) ] <- ( TH7 ) [ !(STUD == 21) & (STUD == 25) ]",
    "FUD [ !(STUD == 21) & (STUD == 25) ] <- ( TH7 ) [ !(STUD == 21) & (STUD == 25) ]",
    "FUD [ !(STUD == 21) & !(STUD == 25) & (STUD == 26) ] <- ( TH8 ) [ !(STUD == 21) & !(STUD == 25) & (STUD == 26) ]",
    "FUD [ !(STUD == 21) & !(STUD == 25) & !(STUD == 26) & (STUD == 83) ] <- ( TH9 ) [ !(STUD == 21) & !(STUD == 25) & !(STUD == 26) & (STUD == 83) ]",
    "FUD [ !(STUD == 21) & !(STUD == 25) & !(STUD == 26) & (STUD == 83) ] <- ( TH9 ) [ !(STUD == 21) & !(STUD == 25) & !(STUD == 26) & (STUD == 83) ]",
    "FUD [ !(STUD == 21) & !(STUD == 25) & !(STUD == 26) & !(STUD == 83) & (STUD == 85) ] <- ( TH10 ) [ !(STUD == 21) & !(STUD == 25) & !(STUD == 26) & !(STUD == 83) & (STUD == 85) ]",
    "TEST [ !(STUD == 21) & !(STUD == 25) & !(STUD == 26) & !(STUD == 83) & !(STUD == 85) ] <- 1",
    "FUD [ !(STUD == 21) & !(STUD == 25) & !(STUD == 26) & !(STUD == 83) & !(STUD == 85) ] <- ( TH11 ) [ !(STUD == 21) & !(STUD == 25) & !(STUD == 26) & !(STUD == 83) & !(STUD == 85) ]"
  )

  expect_true(all(parsePredCode(ifElseText) == ifElseTarget))
})
