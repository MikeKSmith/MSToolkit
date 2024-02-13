setEctdDataMethod("CSV")
resetEctdColNames()

test.analyseData.BasicAnalysis <- function(){

	systemTestPath <- system.file(package = "MSToolkit", "systemTest", "data")
	interimPath <- file.path(systemTestPath, "Interim")
	scriptsPath <- file.path(systemTestPath, "Scripts")
	
	# Allow analysis of all, or a subset of, generated simulated data from a single function call
	setEctdDataMethod("CSV")
	whichPath <- interimPath	
	lmCode <- function(data) {
		myLm <- lm(RESP ~ DOSE, data = data)
		newData <- data.frame(DOSE = sort(unique(data$DOSE)))
		doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
		names(doseMeans) <- c("Mean", "Lower", "Upper")
		data.frame(DOSE = sort(unique(data$DOSE)), doseMeans, N = as.vector(table(data$DOSE)))
	}
			
	iCode <- function(data, uniDoses = c(0, 5, 25, 50, 100)) {
		zeroUpper <- data$Upper[1]
		whichLower <- data$Lower < zeroUpper
		whichLower[1] <- FALSE
		if (any(whichLower)) list(DROP = uniDoses [ whichLower ])
		else list()
	}

	mCode <- function(data, doseCol, interimCol) {
		data.frame(DROPPED = sum(data$DROPPED), TEST = 1)
	}
	
	# Delete Files
	targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
	
	analyzeData(replicates = 1:3, analysisCode = lmCode, macroCode = mCode, interimCode = iCode, workingPath = whichPath)
	
	checkTrue(all(file.exists(file.path(whichPath, targetFiles))))
	microFiles <- list.files(file.path(whichPath, "MicroEvaluation"))
	macroFiles <- list.files(file.path(whichPath, "MacroEvaluation"))
	checkTrue( all(microFiles == paste("micro000", 1:3, ".csv", sep="")) )
	checkTrue( all(macroFiles == paste("macro000", 1:3, ".csv", sep=""))) 
	
	getMicro <- read.csv(file.path(whichPath, "microSummary.csv"))
	getMacro <- read.csv(file.path(whichPath, "macroSummary.csv"))
	checkTrue(nrow(getMacro) == 3 & all(getMacro$Replicate == 1:3) & all(getMacro$DROPPED == 1) & all(getMacro$TEST == 1))

	splitMicro <- lapply(split(getMicro[-1], getMicro$Replicate), function(df) {
				row.names(df) <- 1:nrow(df)
				df
			})
	checkTrue(length(splitMicro) == 3)
	checkTrue(identical(splitMicro[[1]], splitMicro[[3]]))
	
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
}

test.analyseData.RScript <- function(){
	
	systemTestPath <- system.file(package = "MSToolkit", "systemTest", "data")
	interimPath <- file.path(systemTestPath, "Interim")
	scriptsPath <- file.path(systemTestPath, "Scripts")

	# Allow analysis based on an R function, external R script or 
	# external SAS script   
	
	# "Based on an R Function" is tested in previous 
	# test, so just need to focus on scripts here
	
	# Setup
	whichPath <- scriptsPath
	setEctdDataMethod("CSV")
	
	iCode <- function(data, uniDoses = c(0, 5, 25, 50, 100)) {
		zeroUpper <- data$Upper[1]
		whichLower <- data$Lower < zeroUpper
		whichLower[1] <- FALSE
		list(KEEP = !whichLower)
	}
	
	mCode <- function(data, doseCol, interimCol) {
		data.frame(DROPPED = sum(data$DROPPED), TEST = 1)
	}
	
	targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
	
	#
	# R Script Calling ...
	#
	
	# Delete Files
	analyzeData(analysisCode = "rAnalysisScript.R", 
			macroCode = mCode, interimCode = iCode, workingPath = whichPath)
	
	getMicro <- read.csv(file.path(whichPath, "microSummary.csv"))
	getMacro <- read.csv(file.path(whichPath, "macroSummary.csv"))
	checkTrue(nrow(getMacro) == 3 & nrow(getMicro) == 60)
	checkTrue(!any(is.na(getMicro$Mean)))
	
	# Clean up files before next go
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
}

test.analyseData.SASScript <- function(){
	
	systemTestPath <- system.file(package = "MSToolkit", "systemTest", "data")
	interimPath <- file.path(systemTestPath, "Interim")
	scriptsPath <- file.path(systemTestPath, "Scripts")

	# Check for SAS existance
	isWin <- .Platform$OS.type == "windows"
	getPath <- if (isWin) getEctdExternalPath("SASPATH_WIN") else getEctdExternalPath("SASPATH_UNIX") 
	if (nchar(getPath) > 1 && file.exists(getPath)) {
		
		# Setup
		whichPath <- scriptsPath
		setEctdDataMethod("CSV")
		
		iCode <- function(data, uniDoses = c(0, 5, 25, 50, 100)) {
			zeroUpper <- data$Upper[1]
			whichLower <- data$Lower < zeroUpper
			whichLower[1] <- FALSE
			list(KEEP = !whichLower)
		}
		
		mCode <- function(data, doseCol, interimCol) {
			data.frame(DROPPED = sum(data$DROPPED), TEST = 1)
		}
		
		targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
		if (any(file.exists(file.path(whichPath, targetFiles)))) {
			for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
		}
				
		analyzeData(analysisCode = "analysisCode.sas", software = "SAS",
				macroCode = mCode, interimCode = iCode, workingPath = whichPath)
		
		getMicro <- read.csv(file.path(whichPath, "microSummary.csv"))
		getMacro <- read.csv(file.path(whichPath, "macroSummary.csv"))
		checkTrue(nrow(getMacro) == 3 & nrow(getMicro) == 60)
		checkTrue(!any(is.na(getMicro$Mean)))
		
		# Clean up files before next go
		if (any(file.exists(file.path(whichPath, targetFiles)))) {
			for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
		}
	}
	else checkTrue(FALSE, " (CAN NOT TEST > 'SAS' NOT FOUND)")
}

test.analyseData.Interims <- function(){
	
	systemTestPath <- system.file(package = "MSToolkit", "systemTest", "data")
	interimPath <- file.path(systemTestPath, "Interim")
	scriptsPath <- file.path(systemTestPath, "Scripts")
	
	# Allow automated interim analysis step and data update based on of microevluation outputs   
	whichPath <- interimPath
	setEctdDataMethod("CSV")
	
	lmCode <- function(data) {
		myLm <- lm(RESP ~ DOSE, data = data)
		newData <- data.frame(DOSE = sort(unique(data$DOSE)))
		doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
		names(doseMeans) <- c("Mean", "Lower", "Upper")
		data.frame(DOSE = sort(unique(data$DOSE)), doseMeans, N = as.vector(table(data$DOSE)))
	}
	
	iCode <- function(data, uniDoses = c(0, 5, 25, 50, 100)) {
		zeroUpper <- data$Upper[1]
		whichLower <- data$Lower < zeroUpper
		whichLower[1] <- FALSE
		list(KEEP = !whichLower)
	}
	
	mCode <- function(data, doseCol, interimCol) {
		data.frame(DROPPED = sum(data$DROPPED), TEST = 1)
	}
	
	# Delete Files
	targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
	
	analyzeData(replicates = 1:3, analysisCode = lmCode, macroCode = mCode, interimCode = iCode, workingPath = whichPath)
	
	checkTrue(all(file.exists(file.path(whichPath, targetFiles))))
	microFiles <- list.files(file.path(whichPath, "MicroEvaluation"))
	macroFiles <- list.files(file.path(whichPath, "MacroEvaluation"))
	checkTrue(all(microFiles == paste("micro000", 1:3, ".csv", sep="")))
	checkTrue(all(macroFiles == paste("macro000", 1:3, ".csv", sep="")))
	
	getMicro <- read.csv(file.path(whichPath, "microSummary.csv"))
	getMacro <- read.csv(file.path(whichPath, "macroSummary.csv"))
	checkTrue(nrow(getMacro) == 3 & all(getMacro$Replicate == 1:3) & all(getMacro$DROPPED == 1) & all(getMacro$TEST == 1))
	splitMicro <- lapply(split(getMicro[-1], getMicro$Replicate), function(df) {
				row.names(df) <- 1:nrow(df)
				df
			})
	checkTrue(length(splitMicro) == 3)
	checkTrue(identical(splitMicro[[1]], splitMicro[[3]]))
	
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
}
	
test.analyseData.MacroEval <- function(){
		
	systemTestPath <- system.file(package = "MSToolkit", "systemTest", "data")
	interimPath <- file.path(systemTestPath, "Interim")
	scriptsPath <- file.path(systemTestPath, "Scripts")
		
	# Allow automated macro evaluation of microevaluation outputs   
	whichPath <- interimPath
	setEctdDataMethod("CSV")
	
	lmCode <- function(data) {
		myLm <- lm(RESP ~ DOSE, data = data)
		newData <- data.frame(DOSE = sort(unique(data$DOSE)))
		doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
		names(doseMeans) <- c("Mean", "Lower", "Upper")
		data.frame(DOSE = sort(unique(data$DOSE)), doseMeans, N = as.vector(table(data$DOSE)))
	}
	
	iCode <- function(data, uniDoses = c(0, 5, 25, 50, 100)) {
		zeroUpper <- data$Upper[1]
		whichLower <- data$Lower < zeroUpper
		whichLower[1] <- FALSE
		list(KEEP = !whichLower)
	}
	
	mCode <- function(data, doseCol, interimCol) {
		data.frame(DROPPED = sum(data$DROPPED), TEST = 1)
	}
	
	# Delete Files
	targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
	
	analyzeData(replicates = 1:3, analysisCode = lmCode, macroCode = mCode, interimCode = iCode, workingPath = whichPath)
	
	checkTrue(all(file.exists(file.path(whichPath, targetFiles))))
	microFiles <- list.files(file.path(whichPath, "MicroEvaluation"))
	macroFiles <- list.files(file.path(whichPath, "MacroEvaluation"))
	checkTrue(all(microFiles == paste("micro000", 1:3, ".csv", sep="")))
	checkTrue(all(macroFiles == paste("macro000", 1:3, ".csv", sep="")))
	
	getMicro <- read.csv(file.path(whichPath, "microSummary.csv"))
	getMacro <- read.csv(file.path(whichPath, "macroSummary.csv"))
	checkTrue(nrow(getMacro) == 3 & all(getMacro$Replicate == 1:3) & all(getMacro$DROPPED == 1) & all(getMacro$TEST == 1))
	splitMicro <- lapply(split(getMicro[-1], getMicro$Replicate), function(df) {
				row.names(df) <- 1:nrow(df)
				df
			})
	checkTrue(length(splitMicro) == 3)
	checkTrue(identical(splitMicro[[1]], splitMicro[[3]]))
	
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
}

test.analyseData.gridCalling <- function() {
	
	systemTestPath <- system.file(package = "MSToolkit", "systemTest", "data")
	interimPath <- file.path(systemTestPath, "Interim")
	scriptsPath <- file.path(systemTestPath, "Scripts")
	
	if (MSToolkit:::.checkGridAvailable()) {
		whichPath <- interimPath
		lmCode <- function(data) {
			myLm <- lm(RESP ~ DOSE, data = data)
			newData <- data.frame(DOSE = sort(unique(data$DOSE)))
			doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
			names(doseMeans) <- c("Mean", "Lower", "Upper")
			data.frame(DOSE = sort(unique(data$DOSE)), doseMeans, N = as.vector(table(data$DOSE)))
		}
		
		iCode <- function(data, uniDoses = c(0, 5, 25, 50, 100)) {
			zeroUpper <- data$Upper[1]
			whichLower <- data$Lower < zeroUpper
			whichLower[1] <- FALSE
			list(KEEP = !whichLower)
			Sys.sleep(1)
		}
		
		mCode <- function(data, doseCol, interimCol) {
			data.frame(DROPPED = sum(data$DROPPED), TEST = 1)
		}
		
		# Delete Files
		targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
		if (any(file.exists(file.path(whichPath, targetFiles)))) {
			for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
		}
		
		analyzeData(replicates = 1:6, analysisCode = lmCode, macroCode = mCode, interimCode = iCode, workingPath = whichPath, grid = TRUE)
		microFiles <- file.info(dir(file.path(whichPath, "MicroEvaluation"), full.names = TRUE))
		orderTime <- order(microFiles$mtime)
		orderName <- order(rownames(microFiles))
		checkTrue(!all(orderTime == orderName ))
	} else {
		checkTrue(FALSE , " (CAN NOT TEST - 'GRID' NOT AVAILABLE)")
	}  

}

test.analyseData.MicroMacroFiles <- function(){
	
	systemTestPath <- system.file(package = "MSToolkit", "systemTest", "data")
	interimPath <- file.path(systemTestPath, "Interim")
	scriptsPath <- file.path(systemTestPath, "Scripts")
	
	# Allow compilation microevalation and macroevaluation, along with writing to external CSV files   
	whichPath <- interimPath
	setEctdDataMethod("CSV")
	
	lmCode <- function(data) {
		myLm <- lm(RESP ~ DOSE, data = data)
		newData <- data.frame(DOSE = sort(unique(data$DOSE)))
		doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
		names(doseMeans) <- c("Mean", "Lower", "Upper")
		data.frame(DOSE = sort(unique(data$DOSE)), doseMeans, N = as.vector(table(data$DOSE)))
	}
	
	iCode <- function(data, uniDoses = c(0, 5, 25, 50, 100)) {
		zeroUpper <- data$Upper[1]
		whichLower <- data$Lower < zeroUpper
		whichLower[1] <- FALSE
		list(KEEP = !whichLower)
	}
	
	mCode <- function(data, doseCol, interimCol) {
		data.frame(DROPPED = sum(data$DROPPED), TEST = 1)
	}
	
	# Delete Files
	targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
	
	analyzeData(replicates = 1:3, analysisCode = lmCode, macroCode = mCode, interimCode = iCode, workingPath = whichPath)
	
	checkTrue(all(file.exists(file.path(whichPath, targetFiles))))
	microFiles <- list.files(file.path(whichPath, "MicroEvaluation"))
	macroFiles <- list.files(file.path(whichPath, "MacroEvaluation"))
	checkTrue(all(microFiles == paste("micro000", 1:3, ".csv", sep="")))
	checkTrue(all(macroFiles == paste("macro000", 1:3, ".csv", sep="")))
	
	getMicro <- read.csv(file.path(whichPath, "microSummary.csv"))
	getMacro <- read.csv(file.path(whichPath, "macroSummary.csv"))
	checkTrue(nrow(getMacro) == 3 & all(getMacro$Replicate == 1:3) & all(getMacro$DROPPED == 1) & all(getMacro$TEST == 1))
	splitMicro <- lapply(split(getMicro[-1], getMicro$Replicate), function(df) {
				row.names(df) <- 1:nrow(df)
				df
			})
	checkTrue(length(splitMicro) == 3)
	checkTrue(identical(splitMicro[[1]], splitMicro[[3]]))
	
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
}

test.analyseData.OmitRows <- function(){
	
	systemTestPath <- system.file(package = "MSToolkit", "systemTest", "data")
	interimPath <- file.path(systemTestPath, "Interim")
	scriptsPath <- file.path(systemTestPath, "Scripts")
	
	# Allow automated removal of rows where the missing, 
	# parameter or response flags are set
	whichPath <- interimPath
	setEctdDataMethod("CSV")
	
	lmCode <- function(data) data.frame(DOSE = sort(unique(data$DOSE)), N = as.vector(table(data$DOSE)))
	
	mCode <- function(data, doseCol, interimCol) data.frame(N = sum(data$N))
	
	# Delete Files
	targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
	
	analyzeData(replicates = 4:6, analysisCode = lmCode, macroCode = mCode, workingPath = whichPath, removeMissing = FALSE, removeParOmit = FALSE, removeRespOmit = FALSE)
	getMacro.000 <- read.csv(file.path(whichPath, "macroSummary.csv"))$N[1]
	
	analyzeData(replicates = 4:6, analysisCode = lmCode, macroCode = mCode, workingPath = whichPath, removeMissing = TRUE, removeParOmit = FALSE, removeRespOmit = FALSE)
	getMacro.100 <- read.csv(file.path(whichPath, "macroSummary.csv"))$N[1]
	
	analyzeData(replicates = 4:6, analysisCode = lmCode, macroCode = mCode, workingPath = whichPath, removeMissing = FALSE, removeParOmit = TRUE, removeRespOmit = FALSE)
	getMacro.010 <- read.csv(file.path(whichPath, "macroSummary.csv"))$N[1]
	
	analyzeData(replicates = 4:6, analysisCode = lmCode, macroCode = mCode, workingPath = whichPath, removeMissing = FALSE, removeParOmit = FALSE, removeRespOmit = TRUE)
	getMacro.001 <- read.csv(file.path(whichPath, "macroSummary.csv"))$N[1]
	
	analyzeData(replicates = 4:6, analysisCode = lmCode, macroCode = mCode, workingPath = whichPath)
	getMacro.111 <- read.csv(file.path(whichPath, "macroSummary.csv"))$N[1]
	
	targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
	
	nVals <- c(getMacro.000, getMacro.100, getMacro.010, getMacro.001, getMacro.111)
	checkTrue(all(nVals == c(400, 364, 333, 333, 242)))
}

test.analyseData.RandomSeed <- function(){
	
	systemTestPath <- system.file(package = "MSToolkit", "systemTest", "data")
	interimPath <- file.path(systemTestPath, "Interim")
	scriptsPath <- file.path(systemTestPath, "Scripts")
	
	# Allow control over random number seed for reproducibility   
	whichPath <- interimPath
	setEctdDataMethod("CSV")
	
	lmCode <- function(data) data.frame(DOSE = sort(unique(data$DOSE)), RAND = rnorm(nrow(data)))
	
	mCode <- function(data, doseCol, interimCol) data.frame(MEAN = round(mean(data$RAND), 2))
	
	# Delete Files
	targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
	analyzeData(replicates = 1:2, analysisCode = lmCode, macroCode = mCode, workingPath = whichPath, seed = 111)
	getMacro1 <- read.csv(file.path(whichPath, "macroSummary.csv"))
	
	targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
	
	analyzeData(replicates = 1:2, analysisCode = lmCode, macroCode = mCode, workingPath = whichPath, seed = 111)
	getMacro2 <- read.csv(file.path(whichPath, "macroSummary.csv"))
	
	targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
	
	checkTrue(identical(getMacro1, getMacro2))

}

test.analyseData.ColumnNames <- function(){
	
	systemTestPath <- system.file(package = "MSToolkit", "systemTest", "data")
	interimPath <- file.path(systemTestPath, "Interim")
	scriptsPath <- file.path(systemTestPath, "Scripts")
	
	# Allow identification of key columns within the data, such as 
	# the "Dose", "Missing" and "Interim" columns
	whichPath <- interimPath
	resetEctdColNames()
	
	lmCode1 <- function(data) {
		myLm <- lm(respcol ~ dosecol, data = data)
		newData <- data.frame(dosecol = sort(unique(data$dosecol)))
		doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
		names(doseMeans) <- c("Mean", "Lower", "Upper")
		data.frame(dosecol = sort(unique(data$dosecol)), doseMeans, N = as.vector(table(data$dosecol)))
	}
	
	lmCode2 <- function(data) {
		myLm <- lm(RESP ~ DOSE, data = data)
		newData <- data.frame(DOSE = sort(unique(data$DOSE)))
		doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
		names(doseMeans) <- c("Mean", "Lower", "Upper")
		data.frame(DOSE = sort(unique(data$DOSE)), doseMeans, N = as.vector(table(data$DOSE)))
	}
	
	iCode <- function(data, uniDoses = c(0, 5, 25, 50, 100)) {
		zeroUpper <- data$Upper[1]
		whichLower <- data$Lower < zeroUpper
		whichLower[1] <- FALSE
		list(KEEP = !whichLower)
	}
	
	mCode <- function(data, doseCol, interimCol) {
		data.frame(DROPPED = sum(data$DROPPED), TEST = 1)
	}
	
	# Delete Files
	targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
	
	analyzeData(replicates = 7:8, analysisCode = lmCode1, macroCode = mCode, interimCode = iCode, workingPath = whichPath, 
			doseCol = "dosecol", parOmitFlag = "paromitcol", missingFlag = "missingcol",
			interimCol = "interimcol", respOmitFlag = "respomitcol")
	getMicro1 <- read.csv(file.path(whichPath, "microSummary.csv"))
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
	
	analyzeData(replicates = 5:6, analysisCode = lmCode2, macroCode = mCode, interimCode = iCode, workingPath = whichPath)
	getMicro2 <- read.csv(file.path(whichPath, "microSummary.csv"))
	if (any(file.exists(file.path(whichPath, targetFiles)))) {
		for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
	}
	
	names(getMicro1) <- names(getMicro2)
	checkTrue(identical(getMicro1, getMicro2))

}

