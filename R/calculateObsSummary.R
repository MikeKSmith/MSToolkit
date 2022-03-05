#' Calculate Observed Data Summary
#'
#' Calculate a summary of an "observed" dataset (typically used to overlay
#' observed data in a simulated plot or table)
#'
#' The inputs are checked, and the alpha value is parsed using
#' \link{checkSimAlpha} Subsets are applied to the data if "subset" is
#' specified and differences from baseline are calculated using
#' \link{calculateDiffsFromBase} if required
#'
#' If respType is "Continuous": * Mean responses are calculated by Subject
#' (\code{idCol}), Dose (\code{doseCol}), Time (\code{timeCol}), and any by
#' variables ((\code{bVar}) * The following summaries are then created (based
#' on any by variables (\code{bVar})) - Mean - Median - Minimum - Maximum -
#' Number of non-missing values - Lower alpha\% percent interval - Upper
#' alpha\% percent interval
#'
#' When dealing with categorical responses, it is possible that the unique set
#' of responses could be (say): 1, 2, 4, 5 In this case, it is unclear as to
#' whether the value "3" should be included in a summary.  If "fillRespRange"
#' is TRUE, it would be included (although would have a count/proportion of
#' zero) Frequencies are calculated by "Response level" (\code{respCol}) and
#' any by variables Frequences are converted to proportions (within by variable
#' level) if required (\code{catType})
#'
#' @param data Observed data frame
#' @param respCol Response column names (given by \link{getEctdColName} by
#' default)
#' @param bVar Variables by which summary should be produced (none by default)
#' @param subset Subsets to be applied to the observed data before calculating
#' the summary
#' @param alpha Alpha value for calculation of lower and upper intervals
#' @param digits Number of digits to round summary data
#' @param diffBase Logical: Should differences from baseline be summarised
#' instead of raw data?
#' @param doseCol Dose column names (given by \link{getEctdColName} by default)
#' @param timeCol Time column names (given by \link{getEctdColName} by default)
#' @param idCol Subject column names (given by \link{getEctdColName} by
#' default)
#' @param respType Response type: Continuous (default) or Categorical
#' @param catType For Categorical response, should "Count" (default) or
#' "Proportion" summary be returned?
#' @param fillRespRange For Categorical response, should we "fill" the range of
#' responses (see below)
#' @return A data frame of response summaries
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @keywords Observed
#'
#' @export
"calculateObsSummary" <- function(
		data,
		respCol = getEctdColName("Response"),
		bVar = NULL,
		subset = NULL,
		alpha = 95,
		digits = 3,
		diffBase = FALSE,
		doseCol = getEctdColName("Dose"),
		timeCol = getEctdColName("Time"),
		idCol = getEctdColName("Subject"),
		respType = c("Continuous", "Categorical"),
		catType = c("Count", "Proportion"),
		fillRespRange = TRUE
)
{
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# createObsSummary.R 19NOV09
	#
	# Author: Rich
	###############################################################################
	# DESCRIPTION: Create summary of observed variables from a NONMEM object
	###############################################################################
	# Basic input checks
	respType <- match.arg(respType); catType <- match.arg(catType)
	.checkLogical(diffBase, fillRespRange)							# Check single logicals
	.checkCharacter(respCol, idCol, respType, catType)				# Check single characters
	.checkNumeric(digits)											# Check single numerics

	# Parse the "alpha" input
	alpha <- checkSimAlpha(alpha)
	alpha <- qnorm((1 + alpha)/2)

	# Get the data
	data <- switch(class(data),
			"NMRun" = data,   ## TODO: Call correct data method here
			"data.frame" = data,
			ectdStop("Observed data must be either a data frame or NONMEM run object"))

	# Check inputs
	if (!(timeCol %in% names(data))) timeCol <- NULL
	if (!(doseCol %in% names(data))) doseCol <- NULL
	if (!length(bVar)) ectdStop("Need at least 1 by variable to calculate summaries")
	needCols <- unique(c(respCol, bVar, idCol))
	checkColNames(names(data), needCols)

	# Subset if required
	if (!is.null(subset)) data <- .applyDataSubset(data, subset)
	if (diffBase & length(timeCol)) data <- calculateDiffsFromBase(data, respCol = respCol, idCol = idCol, timeCol = timeCol, replicateCol = NULL)

	# Calculate "means" based on response variable type
	if (respType == "Continuous") {
		# Calculate means by subject, time and by variables
		meanData <- .dataAggregate(data[respCol], data[unique(c(idCol, doseCol, timeCol, bVar))], mean, na.rm = TRUE)

		#
		# Set up functions to use in the aggregate calls
		#
		loFun <- function(x, alp, dG) round(mean(x, na.rm = T) - (alp * sd(x, na.rm = T))/sqrt(sum(!is.na(x))), dG)
		upFun <- function(x, alp, dG) round(mean(x, na.rm = T) + (alp * sd(x, na.rm = T))/sqrt(sum(!is.na(x))), dG)
		meanFun <- function(x, dG) round(mean(x, na.rm = T), dG)
		medianFun <- function(x, dG) round(median(x, na.rm = T), dG)
		minFun <- function(x, dG) round(min(x, na.rm = T), dG)
		maxFun <- function(x, dG) round(max(x, na.rm = T), dG)

		#
		# Create summaries
		#
		myMeds <- .dataAggregate(list(Median = meanData[[respCol]]), meanData[bVar], medianFun, dG = digits)
		myMeans <- .dataAggregate(list(Mean = meanData[[respCol]]), meanData[bVar], meanFun, dG = digits)
		myLower <- .dataAggregate(list(Lower = meanData[[respCol]]), meanData[bVar], loFun, dG = digits, alp = alpha)
		myUpper <- .dataAggregate(list(Upper = meanData[[respCol]]), meanData[bVar], upFun, dG = digits, alp = alpha)
		myN <- .dataAggregate(list(N = meanData[[respCol]]), meanData[bVar], function(x) sum(!is.na(x)))
		myMin <- .dataAggregate(list(Min = meanData[[respCol]]), meanData[bVar], minFun, dG = digits)
		myMax <- .dataAggregate(list(Max = meanData[[respCol]]), meanData[bVar], maxFun, dG = digits)

		# Merge summaries
		outData <- merge(myMeds, merge(myMeans, merge(myLower, merge(myUpper, merge(myMin, merge(myMax, myN))))))
	}
	else {
		# Calculate counts by subject, time, by variables AND response
		meanData <- .dataAggregate(list(COUNT = data[[respCol]]), data[unique(c(bVar, respCol))], function(x) sum(!is.na(x)))
		# "Expand" this to a full set of responses
		expData <- unique(data[bVar])
		uniResp <- unique(data[[respCol]])
		if (fillRespRange & all(is.integer(uniResp))) uniResp <- min(uniResp):max(uniResp)
		fullData <- expData[ rep(1:nrow(expData), length(uniResp)), , drop = FALSE]
		fullData[[respCol]] <- rep(uniResp, each = nrow(expData))
		meanData <- merge(meanData, fullData, all = TRUE)
		outData <- meanData [ do.call("order", meanData[unique(c(bVar, respCol))]), , drop = FALSE]
		outData$COUNT <- replace(outData$COUNT, is.na(outData$COUNT), 0)
		if (catType == "Proportion") {
			totalData <- .dataAggregate(list(TOTAL = outData$COUNT), outData[bVar], sum, na.rm = TRUE)
			outData <- merge(outData, totalData)
			outData$PROPORTION <- round(100 * outData$COUNT / outData$TOTAL, digits = digits)
			outData <- outData[setdiff(names(outData), c("COUNT", "TOTAL"))]
		}
	}
	return(outData)
}
