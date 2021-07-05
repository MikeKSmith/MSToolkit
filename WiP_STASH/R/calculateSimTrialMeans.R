#' Calculate Simulated Trial Means
#' 
#' Calculate mean simulated responses by replicate
#' 
#' The inputs are checked, and the alpha value is parsed using
#' \link{checkSimAlpha} Subsets are applied to the data if "subset" is
#' specified and differences from baseline are calculated using
#' \link{calculateDiffsFromBase} if required
#' 
#' If respType is "Continuous": * Mean responses are calculated by Replicate
#' (\code{replicateCol}), Subject (\code{idCol}), Dose (\code{doseCol}), Time
#' (\code{timeCol}), and any by variables (\code{bVar}) * Means of these means
#' are then calculated by Replicate (\code{replicateCol}) and any by variables
#' (\code{bVar}) * The data is rounded (\code{digits}) and returned
#' 
#' When dealing with categorical responses, it is possible that the unique set
#' of responses could be (say): 1, 2, 4, 5 In this case, it is unclear as to
#' whether the value "3" should be included in a summary.  If "fillRespRange"
#' is TRUE, it would be included (although would have a count/proportion of
#' zero) Frequencies are calculated by "Response level" (\code{respCol}) and
#' any by variables Frequences are converted to proportions (within by variable
#' level) if required (\code{catType})
#' 
#' @param data Simulated trial data frame
#' @param respCol Response column names (given by \link{getEctdColName} by
#' default)
#' @param bVar Variables by which means should be produced ("Dose" by default)
#' @param subset Subsets to be applied to the data before calculating the means
#' @param diffBase Logical: Should differences from baseline be summarised
#' instead of raw data?
#' @param idCol Subject column names (given by \link{getEctdColName} by
#' default)
#' @param timeCol Time column names (given by \link{getEctdColName} by default)
#' @param doseCol Dose column names (given by \link{getEctdColName} by default)
#' @param replicateCol Replicate column names (given by \link{getEctdColName}
#' by default)
#' @param respType Response type: Continuous (default) or Categorical
#' @param catType For Categorical response, should "Count" (default) or
#' "Proportion" summary be returned?
#' @param fillRespRange For Categorical response, should we "fill" the range of
#' responses (see below)
#' @param digits Number of digits to round summary data
#' @return A data frame of trial "means"
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
"calculateSimTrialMeans" <- function(
		data, 
		respCol = getEctdColName("Response"), 
		bVar = doseCol,
		subset = NULL, 
		diffBase = FALSE, 
		idCol = getEctdColName("Subject"), 
		timeCol = getEctdColName("Time"), 
		doseCol = getEctdColName("Dose"), 
		replicateCol = getEctdColName("Replicate"), 
		respType = c("Continuous", "Categorical"), 
		catType = c("Proportion", "Count"), 
		fillRespRange = TRUE,
		digits = 3)
{
	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# calculateSimTrialMeans.R 10NOV09
	# 
	# Author: Rich
	###############################################################################
	# DESCRIPTION: Create set of simulated trial means
	###############################################################################

	# Check response and categorical types
	respType <- match.arg(respType)
	catType <- match.arg(catType)
	
	# Basic Input Checks
	.checkLogical(diffBase, fillRespRange)											# Check single logicals
	.checkCharacter(respCol, idCol, doseCol, replicateCol, respType, catType)		# Check single characters
	.checkNumeric(digits)															# Check single numerics
	
	# Check inputs & variables
	if (!is.data.frame(data)) ectdStop("Data frame must be provided as the first input")
	neededCols <- c(replicateCol, idCol, doseCol, respCol)
	if (!all(neededCols %in% names(data))) ectdStop("Some variables not found in input data")
	if (!(timeCol %in% names(data))) timeCol <- c()

	# Differences from baseline
	if (diffBase) data <- calculateDiffsFromBase(data, respCol = respCol, idCol = idCol, timeCol = timeCol, replicateCol = replicateCol)
	if (!missing(subset) && length(subset)) try(data <- .applyDataSubset(data, subset))
	
	# Split based on response type
	if (respType == "Continuous") {
		# First pass - include trial, subject, dose, time and by variables
		calcCols <- unique(c(replicateCol, idCol, doseCol, timeCol, bVar))
		calcCols <- calcCols[calcCols %in% names(data)]
		fullMeans <- .dataAggregate(data[respCol], data[calcCols], mean)
		# Second pass - include trial and by variables
		calcCols <- unique(c(replicateCol, bVar))
		calcCols <- calcCols[calcCols %in% names(data)]
		trialMeans <- .dataAggregate(fullMeans[respCol], fullMeans[calcCols], mean)
		trialMeans[[respCol]] <- round(trialMeans[[respCol]], digits)
		return(trialMeans)
	}
	else {
		# Categorical response
		# Partial lengths
		calcCols <- unique(c(replicateCol, bVar))
		calcCols <- calcCols[calcCols %in% names(data)]
		uniResp <- unique(data[[respCol]])
		if (fillRespRange && all(is.integer(uniResp))) uniResp <- min(uniResp):max(uniResp)
		partialN <- .dataAggregate(list("COUNT" = data[[respCol]]), data[c(calcCols, respCol)], function(x) sum(!is.na(x)))

		# Expanded lengths
		uniData <- unique(partialN[calcCols])
		repData <- uniData [ rep(1:nrow(uniData), length(uniResp)), , drop = FALSE]
		mergeData <- cbind(repData, .RESP = rep(uniResp, each = nrow(uniData)))
		names(mergeData) [ names(mergeData) == ".RESP"] <- respCol
		
		fullData <- merge(mergeData, partialN, all = T)
		if (any(naTest <- is.na(fullData$COUNT))) {
			fullData$COUNT <- replace(fullData$COUNT, naTest, 0)
		}

		# Convert if Proportions required
		if (catType == "Proportion") {
			totalData <- .dataAggregate(list(.TOTAL = fullData$COUNT), fullData[calcCols], sum)
			fullData <- merge(fullData, totalData)
			fullData$PROPORTION <- 100 * fullData$COUNT / fullData$.TOTAL
			fullData$PROPORTION <- round(fullData$PROPORTION, digits)
			fullData <- fullData[setdiff(names(fullData), c("COUNT", ".TOTAL"))]
		}

		# Sort the data
		fullData <- fullData [ do.call("order", fullData[c(calcCols, respCol)]), , drop = FALSE ]
		return(fullData)
	}
}
