
test.data.typicalValue <- function() {

	# Checking exceptions
	checkException( createTvDefinition() )
	checkException( createTvDefinition("A") )
	checkException( createTvDefinition("A", 1) )
	checkException( createTvDefinition("A", 1, 1:3, type = "X") )
	checkException( createTvDefinition(letters, 1, 1:3) )
	checkException( createTvDefinition("A", 1:2, 1:3) )
	checkException( createTvDefinition("A", -1, 1:3) )
	checkException(  createTvDefinition("A", 1, 1:3, type = "C")  )
	checkException(  createTvDefinition("C", 3, 1:3, -1:5, "C", cbind(1:3, 1:3)) )
	
	# Check "working" examples
	out1 <- createTvDefinition("A", 1, 1:3)
	out2 <- createTvDefinition("B", 2, 1:3, -1:5)
	out3 <- createTvDefinition("C", 3, 1:3, -1:5, "C", cbind(rep(1, 5), rep(2, 5)))

	# Now test outputs: first the basics
	checkTrue( out1@Name == "A" & out1@Value == 1 )
	checkTrue( out2@Name == "B" & out2@Value == 2 )
	checkTrue( out3@Name == "C" & out3@Value == 3 )

	# Now the eventual calls to treatment
	out1 <- out1@trtCall
	out2 <- out2@trtCall
	out3 <- out3@trtCall
	
	checkTrue(all(out1$doses == 1:3)); checkTrue(all(out2$doses == 1:3)); checkTrue(all(out3$doses == 1:3))
	checkTrue(is.null(out1$times)); checkTrue(all(out2$times == -1:5)); checkTrue(all(out3$times == -1:5))
	checkTrue(out1$type == "Parallel"); checkTrue(out2$type == "Parallel"); checkTrue(out3$type == "Crossover")
	checkTrue(identical(out3$sequence, cbind(rep(1, 5), 2)))

}

test.data.parsePred <- function() {

	inTest <- c(
			"X1 = THETA(1)^EPS(2)", 
			"X2 = A.GT.0.AND.B.EQ.2.OR.C.LE.5"
	)
	outTest <- c(
			"X1  <-  TH1^EPS2", 
			"X2  <-  (A > 0) & (B == 2) | (C <= 5)"
	)	
	checkTrue(length(parsePredCode(inTest)) == length(outTest))
	checkTrue(all(parsePredCode(inTest) == outTest))
	
	# non-integer values
	inTest <- c(
			"X1 = THETA(1)^EPS(2)", 
			"X2 = A.GT.0.99.AND.B.EQ.2.0.OR.C.LE..51"
	)
	outTest <- c(
			"X1  <-  TH1^EPS2", 
			"X2  <-  (A > 0.99) & (B == 2.0) | (C <= .51)"
	)	
	parsePredCode(inTest)
	checkTrue(length(parsePredCode(inTest)) == length(outTest))
	checkTrue(all(parsePredCode(inTest) == outTest))
	
	inTest <- c(
			"X1=EXP(THETA(1))", 
			"X2=LOG(THETA(2))",
			"X3=ABS(ETA(1))**2",
			"Y=SQRT(ETA(2)) * SIN(EPA(1))"
	)
	outTest <- c(
			"X1 <- exp( TH1)", 
			"X2 <- log( TH2)", 
			"X3 <- abs( ETA1) ^ 2", 
			"Y <- sqrt( ETA2)  *  sin( EPA(1))", 
			"RESP <- Y  # Additional Command Added"
	)	
	checkTrue(all(parsePredCode(inTest) == outTest))
	
	
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
			
	checkTrue(all(parsePredCode(ifElseText) == ifElseTarget))

}

test.data.applyPredCode <- function() {
	
	myDf <- data.frame(X = 1:5, TH1 = rep(1, 5), TH2 = rep(2, 5), ETA1 = rep(3, 5), EPS1 = rep(4, 5))
	predCode <- c(
			"TEST = 1",
			"XCOPY = X",
			"TH2COPY = THETA(2)",
			"Y = XCOPY + LOG(THETA(1)) + THETA(2)**2 + ETA(1) + SQRT(EPS(1)) + 1"
	)
	outDf <- applyPredCode(myDf, parsePredCode(predCode), "RESP", FALSE, c("TEST", "XCOPY", "TH2COPY", "RESP"))
	
	myDf$TEST <- rep(1, 5); myDf$XCOPY <- myDf$X; myDf$TH2COPY <- myDf$TH2
	myDf$RESP <- with(myDf, XCOPY + log(TH1) + TH2^2 + ETA1 + sqrt(EPS1) + 1)
	checkTrue(identical(outDf, myDf))
	
}