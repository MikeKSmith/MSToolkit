
test.diffsFromBaseline <- function() {

	# Generate some test data
	myDf <- expand.grid(xReplicate = 1:2, xSUBJ = 1:5, xTIME = -1:2)
	myDf$xRESP <- myDf$xTIME + 2
	myDf <- myDf [ do.call("order", myDf[c("xReplicate", "xSUBJ", "xTIME")]),]
	oneSubjectNot <- with(myDf, xReplicate == 1 & xSUBJ == 1 & xTIME <= 0)
	setEctdColName("Subject", "xSUBJ")
	setEctdColName("Replicate", "xReplicate")
	setEctdColName("Time", "xTIME")
	setEctdColName("Response", "xRESP")

	# Calling Exceptions
	nowWarn <- options()$warn
	options(warn = 2)
	checkException(calculateDiffsFromBase(1:5), "Calling with incorrect first input")
	checkException(calculateDiffsFromBase(myDf, idCol = "X"), "Calling with incorrect subject variable name")
	checkException(calculateDiffsFromBase(myDf, timeCol = "X"), "Calling with incorrect time variable name")
	checkException(calculateDiffsFromBase(myDf, respCol = "X"), "Calling with incorrect response variable name")
	checkException(calculateDiffsFromBase(myDf, repColCol = "X"), "Calling with incorrect response variable name")
	checkException(calculateDiffsFromBase(myDf, baseDef = "..XX"), "Incorrect input for baseline definition")
	checkException(calculateDiffsFromBase(myDf, baseDef = "xTIME < -5"), "No baseline data")
	checkException(calculateDiffsFromBase(myDf, baseDef = "xTIME < 5"), "All baseline data")
	checkException(calculateDiffsFromBase(myDf[!oneSubjectNot,]), "One subject without baseline")
	options(warn = nowWarn)

	# These should work fine
	out1 <- calculateDiffsFromBase(myDf)
	out2 <- calculateDiffsFromBase(myDf, tolerance = 1.6)
	out3 <- calculateDiffsFromBase(myDf, removeBaseline = FALSE)
	sub1 <- out1 [ out1$xReplicate == 1 & out1$xSUBJ == 1, ]$xRESP
	sub2 <- out2 [ out2$xReplicate == 1 & out2$xSUBJ == 1, ]$xRESP
	sub3 <- out3 [ out3$xReplicate == 1 & out3$xSUBJ == 1, ]$xRESP
	checkTrue(length(sub1) == 2 && all(sub1 == c(1.5, 2.5)), "Correct call")
	checkTrue(length(sub2) == 2 && all(sub2 == c(0, 2.5)), "Correct call with tolerance")
	checkTrue(length(sub3) == 4 && all(sub3 == c(-.5, .5, 1.5, 2.5)), "Correct call keeping baseline")
	
	# Reset to default column names
	resetEctdColNames()
	
}

test.checkSimAlpha <- function() {
		
	# Test proving multiple alphas (numeric)
	checkException(checkSimAlpha(1:10), "Fail on multiple numeric inputs")
	checkException(checkSimAlpha(letters), "Fail on multiple character inputs")

	# Character inputs
	checkTrue(checkSimAlpha("95%") == .95, "Character with percent")
	checkTrue(checkSimAlpha("9 5 % ") == .95, "Spaced character with percent")
	checkTrue(checkSimAlpha(" 95 ") == .95, "Spaced character without percent")
	checkException(checkSimAlpha(" hello "), "Invalid character input")

	# Check range of inputs (numeric)
	testVec <- seq(50, 100, by=.5)
	for (i in testVec) checkTrue(checkSimAlpha(i) == i/100, paste("Check that '", i, "' parses correctly", sep=""))	
	
	testVec <- seq(1.5, 50, by=.5)
	for (i in testVec) checkTrue(checkSimAlpha(i) == (1-i/100), paste("Check that '", i, "' parses correctly", sep=""))	

}

test.calculateSimTrialMeans <- function() {
	
	# Example data
	simData <- expand.grid(TRIAL = 1:2, DOSE=c(0, 5, 15), GEN = 0:1, AGE = c(18, 35, 45, 60))
	N <- nrow(simData); simData$ID <- 1:N
	simData <- merge(simData, expand.grid(ID = 1:N, TIME = 1:3))
	simData$RESP <- rnorm(nrow(simData))
	
	# Check exceptions
	checkException(calculateSimTrialMeans( simData, respType = "a", respCol = "AGE", idCol = "ID", timeCol = "TIME", doseCol = "DOSE", replicateCol = "TRIAL"))
	checkException(calculateSimTrialMeans( simData, catType = "a", respCol = "AGE", idCol = "ID", timeCol = "TIME", doseCol = "DOSE", replicateCol = "TRIAL"))
	checkException(calculateSimTrialMeans( simData, diffBase = 1, respCol = "AGE", idCol = "ID", timeCol = "TIME", doseCol = "DOSE", replicateCol = "TRIAL"))
	checkException(calculateSimTrialMeans( simData, fillRespRange = 1, respCol = "AGE", idCol = "ID", timeCol = "TIME", doseCol = "DOSE", replicateCol = "TRIAL"))
	checkException(calculateSimTrialMeans( simData, respCol = 1, idCol = "ID", timeCol = "TIME", doseCol = "DOSE", replicateCol = "TRIAL"))
	checkException(calculateSimTrialMeans( simData, respCol = "RESP", idCol = letters, doseCol = "DOSE", replicateCol = "TRIAL"))
	checkException(calculateSimTrialMeans( simData, respCol = "RESP", idCol = "ID", doseCol = TRUE, replicateCol = "TRIAL"))
	checkException(calculateSimTrialMeans( simData, respCol = "RESP", idCol = "ID", doseCol = "DOSE", replicateCol = NULL))
	checkException(calculateSimTrialMeans( simData, respCol = "RESP", idCol = "ID", doseCol = "DOSE", replicateCol = "TRIAL", digits = "hello"))
	checkException(calculateSimTrialMeans( 1:5, respCol = "RESP", idCol = "ID", doseCol = "DOSE", replicateCol = "TRIAL"))
	checkException(calculateSimTrialMeans( data.frame(X = 1:5), respCol = "RESP", idCol = "ID", doseCol = "DOSE", replicateCol = "TRIAL"))

	# Check calculation is correct
	simData <- expand.grid(TRIAL = 1:2, DOSE=c(0, 5, 15), GEN = 0:1, AGE = c(18, 35, 45, 60))
	N <- nrow(simData); simData$ID <- 1:N
	simData <- merge(simData, expand.grid(ID = 1:N, TIME = 1:3))
	simData$RESP <- rnorm(nrow(simData))
	out1 <- calculateSimTrialMeans( simData, respCol = "AGE", replicateCol = "TRIAL",
			doseCol = "DOSE", timeCol = "TIME", idCol = "ID")
	checkTrue(all(dim(out1) == c(6, 3)) && all(names(out1) == c("TRIAL", "DOSE", "AGE")))
	checkTrue(all(out1$AGE == 39.5))

	# Test naming of variables
	simData <- expand.grid(.TRIAL = 1:2, .ID = 1, .TIME = 1:2, .DOSE=c(5, 15), .GEN = 0:1)
	simData$.RESP <- rnorm(nrow(simData))
	out <- calculateSimTrialMeans( simData, respCol = ".RESP", replicateCol = ".TRIAL", 
			idCol = ".ID", bVar = ".GEN", doseCol = ".DOSE", timeCol = ".TIME")					
	checkTrue(all(names(out) == c(".TRIAL", ".GEN", ".RESP")))

	# Test random number generation
	simData <- expand.grid(TRIAL = 1:2, DOSE=c(0, 5, 15), GEN = 0:1, AGE = c(18, 35, 45, 60))
	N <- nrow(simData); simData$ID <- 1:N
	simData <- merge(simData, expand.grid(ID = 1:N, TIME = 1:3))
	simData$RESP <- rnorm(nrow(simData))				
	out1 <- calculateSimTrialMeans( simData, respCol = "RESP", replicateCol = "TRIAL", 
			idCol = "ID", bVar = "GEN", doseCol = "DOSE", timeCol = "TIME", digits = 10 )					
	byHand <- aggregate(list(RESP = simData$RESP), simData[c("TRIAL", "ID", "DOSE", "TIME", "GEN")], mean)
	byHand <- aggregate(list(RESP = byHand$RESP), byHand[c("TRIAL", "GEN")], mean)
	checkTrue(all(round(out1$RESP, 4) == round(byHand$RESP, 4)))

	# Check rounding
	simData <- expand.grid(TRIAL = 1:2, DOSE=c(0, 5, 15), GEN = 0:1, AGE = c(18, 35, 45, 60))
	N <- nrow(simData); simData$ID <- 1:N
	simData <- merge(simData, expand.grid(ID = 1:N, TIME = 1:3))
	simData$RESP <- rnorm(nrow(simData))			
	out1 <- calculateSimTrialMeans( simData, respCol = "RESP", replicateCol = "TRIAL", 
			idCol = "ID", bVar = "GEN", doseCol = "DOSE", timeCol = "TIME", digits = 10 )					
	out2 <- calculateSimTrialMeans( simData, respCol = "RESP", replicateCol = "TRIAL", 
			idCol = "ID", bVar = "GEN", doseCol = "DOSE", timeCol = "TIME", digits = 4 )					
	out3 <- calculateSimTrialMeans( simData, respCol = "RESP", replicateCol = "TRIAL", 
			idCol = "ID", bVar = "GEN", doseCol = "DOSE", timeCol = "TIME", digits = 2 )					
	test1 <- all(out1$RESP != out2$RESP) & all(round(out1$RESP, 4) == out2$RESP)
	test2 <- all(out1$RESP != out3$RESP) & all(round(out1$RESP, 2) == out3$RESP)
	checkTrue(test1 & test2)

	# Check subsetting + diffBase
	simData <- expand.grid(TRIAL = 1:2, DOSE=c(0, 5, 15), GEN = 0:1, AGE = c(18, 35, 45, 60))
	N <- nrow(simData); simData$ID <- 1:N
	simData <- merge(simData, expand.grid(ID = 1:N, TIME = -1:3))
	simData$RESP <- rnorm(nrow(simData))
	simData$RESP [ simData$TIME <= 0 ] <- 1
	out1 <- calculateSimTrialMeans( simData, respCol = "RESP", replicateCol = "TRIAL", 
			idCol = "ID", bVar = "GEN", doseCol = "DOSE", timeCol = "TIME", diffBase= FALSE, subset = "TIME > 0")$RESP
	out2 <- calculateSimTrialMeans( simData, respCol = "RESP", replicateCol = "TRIAL", 
			idCol = "ID", bVar = "GEN", doseCol = "DOSE", timeCol = "TIME", diffBase= TRUE)$RESP
	out3 <- calculateSimTrialMeans( simData, respCol = "RESP", replicateCol = "TRIAL", 
			idCol = "ID", bVar = "GEN", doseCol = "DOSE", timeCol = "TIME", subset = "AGE > 20" )$RESP					
	out4 <- calculateSimTrialMeans( subset(simData, AGE > 20), respCol = "RESP", replicateCol = "TRIAL", 
			idCol = "ID", bVar = "GEN", doseCol = "DOSE", timeCol = "TIME" )$RESP
	checkTrue(all(round(out1 - out2 - 1, 2) == 0))
	checkTrue(all(out3 == out4))

	# Handling of missing categories for summarisation i.e. aggregate-like or tapply-like?
	simData <- expand.grid(TRIAL = 1:2, DOSE=c(0, 5, 15), GEN = 0:1, AGE = c(18, 35, 45, 60))
	N <- nrow(simData); simData$ID <- 1:N
	simData <- merge(simData, expand.grid(ID = 1:N, TIME = -1:3))
	simData$RESP <- rnorm(nrow(simData))
	whichRows <- with(simData, DOSE == 0 & GEN == 0)
	simData <- simData[!whichRows, ]
	out <- calculateSimTrialMeans( simData, respCol = "RESP", replicateCol = "TRIAL", 
			idCol = "ID", bVar = c("GEN", "DOSE"), doseCol = "DOSE", timeCol = "TIME")
	checkTrue( nrow(out) == 10 )
	checkTrue( !any(is.na(out$RESP)))
	
}

test.summarizeTrialMeans <- function() {	
	
	# Generate summary data
	myDf <- data.frame(Y1=rnorm(100), Y2=rnorm(100), Y3=rnorm(100), X1=sample(1:3, 100, T), X2=sample(1:3, 100, T), X3=sample(1:3, 100, T))
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
	checkException(summarizeTrialMeans(1:5), "First input must be a data frame")
	checkException(summarizeTrialMeans(myDf, letters), "Single response variable")
	checkException(summarizeTrialMeans(myDf, "hello"), "Unknown response variable")
	checkException(summarizeTrialMeans(myDf, "Y1", bVar = NULL), "No By Variables")
	checkException(summarizeTrialMeans(myDf, "Y1", bVar = c("X1", "Hello")), "Some By Variables missing")
	checkException(summarizeTrialMeans(myDf, "Y1", bVar = "X1", method = "hello"), "Wrong method")
	checkException(summarizeTrialMeans(myDf, "Y1", bVar = "X1", alpha = "hello"), "Wrong alpha")
		
	# Calculate summaries using summarizeTrialMeans
	qSummaryList <- lapply(1:3, function(i, df, yVars, xVars, alphaVec) summarizeTrialMeans(df, yVars[i], xVars[1:i], alpha=alphaVec[i], digits=i + 1, method="Q"), df=myDf, yVars = yVars, xVars = xVars, alphaVec = alphaVec)
	gSummaryList <- lapply(1:3, function(i, df, yVars, xVars, alphaVec) summarizeTrialMeans(df, yVars[i], xVars[1:i], alpha=alphaVec[i], digits=i + 1, method="G"), df=myDf, yVars = yVars, xVars = xVars, alphaVec = alphaVec)
	
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
		round(c(Median = median(x, na.rm=T), Mean=mean(x, na.rm=T), Lower=as.vector(myLower), Upper=as.vector(myUpper), Min=min(x, na.rm=T), Max=max(x, na.rm=T), N=sum(!is.na(x))), digits)
	}
	
	# Compare created summaries: Quantile method, alpha = 90%, digits = 2
	qData.Y1.90 <- tapply(myDf$Y1, myDf$X1, summaryFun, method="q", alpha=90, digits=2)
	qData.Y1.90 <- matrix(unlist(qData.Y1.90), ncol=7, byrow=T)
	check1 <- as.matrix(qSummaryList[[1]][,-1])
	dimnames(check1) <- NULL
	checkEquals(qData.Y1.90, check1, "Quantile method, alpha 90%, digits 2")

	
	# Compare created summaries: Gaussian method, alpha = 90%, digits = 2
	gData.Y1.90 <- tapply(myDf$Y1, myDf$X1, summaryFun, method="g", alpha=90, digits=2)
	gData.Y1.90 <- matrix(unlist(gData.Y1.90), ncol=7, byrow=T)
	check2 <- as.matrix(gSummaryList[[1]][,-1])
	dimnames(check2) <- NULL
	checkEquals(gData.Y1.90, check2, "Gaussian Method, alpha = 90%, digits = 2")
	
	# Compare created summaries: Quantile method, alpha = 95%, digits = 3
	qData.Y2.95 <- tapply(myDf$Y2, list(myDf$X2, myDf$X1), summaryFun, method="q", alpha=95, digits=3)
	qData.Y2.95 <- matrix(unlist(qData.Y2.95), ncol=7, byrow=T)
	check3 <- as.matrix(qSummaryList[[2]][,-(1:2)])
	dimnames(check3) <- NULL
	checkEquals(qData.Y2.95, check3, "Quantile method, alpha = 95%, digits = 3")
	
	# Compare created summaries: Gaussian method, alpha = 95%, digits = 3
	gData.Y2.95 <- tapply(myDf$Y2, list(myDf$X2, myDf$X1), summaryFun, method="g", alpha=95, digits=3)
	gData.Y2.95 <- matrix(unlist(gData.Y2.95), ncol=7, byrow=T)
	check4 <- as.matrix(gSummaryList[[2]][,-(1:2)])
	dimnames(check4) <- NULL
	checkEquals(gData.Y2.95, check4, "Gaussian method, alpha = 95%, digits = 3")
	
	# Compare created summaries: Quantile method, alpha = 99%, digits = 4
	qData.Y3.99 <- tapply(myDf$Y3, list(myDf$X3, myDf$X2, myDf$X1), summaryFun, method="q", alpha=99, digits=4)
	qData.Y3.99 <- matrix(unlist(qData.Y3.99), ncol=7, byrow=T)
	check5 <- as.matrix(qSummaryList[[3]][,-(1:3)])
	dimnames(check5) <- NULL
	checkEquals(qData.Y3.99, check5, "Quantile method, alpha = 99%, digits = 4")
	
	# Compare created summaries: Gaussian method, alpha = 99%, digits = 4
	gData.Y3.99 <- tapply(myDf$Y3, list(myDf$X3, myDf$X2, myDf$X1), summaryFun, method="g", alpha=99, digits=4)
	gData.Y3.99 <- matrix(unlist(gData.Y3.99), ncol=7, byrow=T)
	check6 <- as.matrix(gSummaryList[[3]][,-(1:3)])
	dimnames(check6) <- NULL
	checkEquals(gData.Y3.99, check6, "Gaussian method, alpha = 99%, digits = 4")

}

test.calculateObsSummary <- function() {	

	# Create test (xpd) objects
	myDf1 <- data.frame(ID = rep(1:6, each = 10), TIME = rep(-1:8, 6), DOSE = rep( rep(c(0, 15, 30), 2), each = 10))
	myDf1$DV <- myDf1$ID + myDf1$DOSE
	myDf2 <- data.frame(SUBJ = rep(1:6, each = 10), DAY = rep(-1:8, 6), DOSE = rep( rep(c(0, 15, 30), 2), each = 10))
	myDf2$DV <- myDf2$SUBJ + myDf2$DAY
	
	# Exceptions
	checkException(calculateObsSummary(), "No arguments")
	checkException(calculateObsSummary(respType = "X"), "Response Type")
	checkException(calculateObsSummary(catType = "X"), "Response Type")
	checkException(calculateObsSummary(diffBase = 1), "DiffBase Flag 1")
	checkException(calculateObsSummary(fillRespRange = "Hello"), "DiffBase Flag 1")
	checkException(calculateObsSummary(diffBase = rep(T, 2)), "DiffBase Flag 2")
	checkException(calculateObsSummary(fillRespRange = rep(T, 2)), "DiffBase Flag 2")
	checkException(calculateObsSummary(respCol = 1), "Resp col 2")
	checkException(calculateObsSummary(respCol = letters), "Resp col 2")
	checkException(calculateObsSummary(idCol = 1), "Subject col 2")
	checkException(calculateObsSummary(idCol = letters), "Subject col 2")
	checkException(calculateObsSummary(digits = "A"), "Digits 1")
	checkException(calculateObsSummary(digits = 1:4), "Digits 2")
	checkException(calculateObsSummary(alpha = "Hello"), "alpha")
	checkException(calculateObsSummary(myDf1, bVar = NULL), "By variables")
	checkException(calculateObsSummary(myDf1, respCol = "NOTINTHERE"), "Response Column")
	checkException(calculateObsSummary(myDf1, respCol = "DV", bVar = "NOTINTHERE"), "Trellis Column")
	checkException(calculateObsSummary(myDf1, respCol = "DV", bVar = "DOSE", idCol = "NOTINTHERE"), "Subject Column")

	# Create test summaries
	os1 <- calculateObsSummary(myDf1, "DV", bVar = "DOSE", subset = "ID < 5", alpha = .9, idCol = "ID", timeCol="TIME")
	os2 <- calculateObsSummary(myDf1, "DV", bVar = "ID", idCol = "ID", timeCol="TIME")
	os3 <- calculateObsSummary(myDf1, "DV", bVar = "TIME", subset = "ID < 5", digits = 2, idCol = "ID", timeCol="TIME")
	os4 <- calculateObsSummary(myDf1, "DV", bVar = "ID", diffBase = T, idCol = "ID", timeCol="TIME")
	os5 <- calculateObsSummary(myDf2, "DV", bVar = "DOSE", diffBase = T, idCol = "SUBJ", timeCol="DAY")
	checkException(calculateObsSummary(myDf1))

	# Test: os1
	checkTrue(all(os1$DOSE == c(0, 15, 30)))
	checkTrue(all(os1$Mean == c(2.5, 17, 33)))
	checkTrue(all(os1$Median == c(2.5, 17, 33)))
	checkTrue(all(round(os1$Lower, 3) == c(1.934, 17, 33)))
	checkTrue(all(round(os1$Upper, 3) == c(3.066, 17, 33)))
	
	# Test: os2
	testData <- unique(myDf1[,c("ID", "DV")])
	checkTrue(all(os2$N == 10))
	checkTrue(all(os2$Mean == testData$DV))
	checkTrue(all(os2$Lower == testData$DV))
	checkTrue(all(os2$Upper == testData$DV))
	checkTrue(all(os2$Min == testData$DV))
	checkTrue(all(os2$Min  == testData$DV))
	
	# Test: os3
	checkTrue(all(os3$TIME == -1:8))
	checkTrue(nrow(unique(os3[-1])) == 1)
	checkTrue(all(unlist(os3[1,-1]) == c(10.5, 13.75, -.55, 28.05, 1, 33, 4)))

	# Test: os4
	checkTrue(all(os4$ID == 1:6))
	checkTrue(nrow(unique(os4[-1])) == 1)
	checkTrue(all(unlist(os4[1,-c(1, 8)]) == 0))
	
	# Test: os5
	checkTrue(all(os5$DOSE == c(0, 15, 30)))
	checkTrue(nrow(unique(os5[-1])) == 1)
	checkTrue(all(unlist(os5[1,-1]) == c(5, 5, 3.84, 6.16, 1.5, 8.5, 16)))

}
