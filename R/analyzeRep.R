#' Analyse a single replicate of data
#'
#' The \code{analyzeRep} function analyzes a single replicate of data, possibly
#' at different interim levels.
#'
#' The \code{\link{analyzeRep}} function calls the
#' \code{\link{performAnalysis}} function in order to analyze and summarize a
#' single simulated replicate dataset (held in the "ReplicateData" subdirectory
#' of the specified working path).
#'
#' The first step of the analysis is to use the removeMissing, removeParOmit
#' and removeRespOmit flags (together with the missingFlag, parOmitFlag and
#' respOmitFlag inputs) in order to subset the data if required.  For example,
#' we may wish to remove all observations flagged as "missing" in an earlier
#' simulation of subject dropout.  The subset is applied to the data before the
#' analysis.
#'
#' The analysis code must be either an R function, a reference to an external R
#' script, or a reference to an external SAS script.  If the software is set as
#' "SAS", it is assumed that the analysisCode is an external SAS script.  If
#' the analysis code is a SAS script, it must accept a single dataset called
#' work.infile, and create an output dataset called work.outfile.  The
#' work.outfile dataset must be a valid "Micro Evaluation" structure as
#' specified in the help file for \code{\link{checkMicroFormat}}.  If the
#' software is "R", the analysis code input must be either an R function or an
#' R script.  The R analysis code must also return a valid "Micro Evaluatoin"
#' structure as specified in function \code{\link{checkMicroFormat}}
#'
#' The first step in \code{\link{analyzeRep}} is to perform a full analysis on
#' the data (which has possibly been subset be the remove* inputs).  Following
#' the analysis, the \code{\link{checkMicroFormat}} function is used to ensure
#' the return data is a valid "Micro Evaluation" data structure.  The return
#' structure is appended with drop and stop flags (set to 0) and with interim
#' variables (where interim is "FULL").
#'
#' If the interimCode has been specified, and the "interimCol" variable is
#' found in the data, interim analyses will be performed iteratively on
#' sections of the data.  The interimCode input must be an R function that
#' returns a suitable list structure as described in the
#' \code{\link{interimAnalysis}} help file.  For each value of "interimCol",
#' the analysis will be performed on a section of data (using a call to
#' \code{\link{performAnalysis}}), and the return from the analysis will be
#' checked (using a call to \code{\link{checkMicroFormat}}).  The "Micro
#' Evaluation" output is then passed to the \code{\link{interimAnalysis}}
#' function and the return list checked for instruction.  If any return interim
#' list includes doses to "DROP", the doses will be removed from future
#' analyses.  If the "STOP" flag in the list is set to "TRUE", the analysis is
#' stopped at this interim.
#'
#' Finally, all micro evaluation outputs (with appended interim variables and
#' drop/stop flags) are combined and returned.
#'
#' @aliases analyseRep
#'
#' @param analysisCode (Required) File containing analysis code (for R or SAS)
#' or an R function for analysis (R only)
#' @param replicate (Required) Replicate number of data to be analyzed
#' @param removeMissing (Optional) Should rows marked as 'Missing' during the
#' data generation step be removed from the data before analysis is performed?
#' TRUE by default
#' @param removeParOmit (Optional) Should any rows marked as 'Omitted' during
#' the parameter data generation step (ie. parameters out of range) be removed
#' from the data before analysis is performed?  TRUE by default
#' @param removeRespOmit (Optional) Should any rows marked as 'Omitted' during
#' the response generation step (ie. responses out of range) be removed from
#' the data before analysis is performed?  TRUE by default
#' @param interimCode (Optional) An R function to be applied to interim
#' datasets in order to creation interim decisions.  See the help file for the
#' \code{\link{interimAnalysis}} function for more information.  By default, no
#' functions is provided, resulting in no interim analyses being performed
#' @param software (Optional) The software to be used for analysis: either "R"
#' or "SAS".  "R" is the default software used
#' @param seed (Optional) Random number seed to use for the analysis.  Based on
#' the current random seed by default
#' @param parOmitFlag (Optional) Parameter omit flag name.  "PAROMIT" by
#' default
#' @param respOmitFlag (Optional) Response omit flag name.  "RESPOMIT" by
#' default
#' @param missingFlag (Optional) Missing flag name.  "MISSING" by default
#' @param interimCol (Optional) Interim variable name.  "INTERIM" by default
#' @param doseCol (Optional) Dose variable name.  "DOSE" by default
#' @param initialDoses (Optional) For interim analyses, which doses should be
#' present in interim 1?  All are included by default
#' @param stayDropped (Optional) For interim analyses, if a dose is dropped,
#' should it stay dropped in following interims (as opposed to allowing the
#' interim step to reopen the dose)
#' @param fullAnalysis (Optional) Should a "full" analysis be performed on all
#' doses?  Default TRUE
#' @param workingPath (Optional) Root directory in which replicate data is
#' stored, and in which we should perform the analysis.  Current working
#' directory is used by default
#' @param method Data storage method (ie. where the replicate data is stored).
#' Given by \link{getEctdDataMethod} by default
#' @return A "Micro Evaluation" structure with additional variables (interim
#' column, drop flag and stop flag)
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso \code{\link{performAnalysis}} is called by \code{analyzeRep} to
#' perform each analysis on the subset of data. \code{\link{interimAnalysis}}
#' executes the \code{interimCode} and updates the data changes accordibgly.
#'
#' \code{\link{analyzeData}} calls \code{analyzeRep} sequentially.
#' @keywords datagen IO models
#' @examples
#'
#' \dontrun{
#'   # Analysis Code
#'   emaxFun <- function(data){
#'     library(DoseResponse)
#'     with( data,
#'      {
#'       uniDoses <- sort( unique(D))
#'       eFit <- emaxalt( RESP, D )
#'       outDf <- data.frame( D = uniDoses,
#'       MEAN = eFit$dm[as.character(uniDoses)],
#'       SE = eFit$dsd[as.character(uniDoses)] )
#'       outDf$LOWER <- outDf$MEAN - 2 * outDf$SE
#'       outDf$UPPER <- outDf$MEAN + 2 * outDf$SE
#'       outDf$N     <- table(DOSE)[ as.character(uniDoses) ]
#'       outDf
#'      })
#'    }
#'
#'   analyzeRep(replicate = 1, analysisCode = emaxFun)
#' }
#'
"analyzeRep" <- function(
		analysisCode		,           #@	File containing the actual analysis code to run on the data
		replicate		,               #@	Replicate number of data to analyze
		removeMissing	= TRUE	,     	#@	Logical flag: remove rows where the "Missing" Flag is set to 1?
		removeParOmit	= TRUE	,     	#@	Logical flag: remove rows where the "Parameter Omit" Flag is set to 1?
		removeRespOmit= TRUE	,     	#@	Logical flag: remove rows where the "Reponse Omit" Flag is set to 1?
		interimCode	= NULL,             #@	Interim analysis Code to run on the data between interims (eg. can be used to drop doses)
		software = c("R", "SAS")	,   	#@	Software system in which the analysis should take place: R or SAS
		seed =	.deriveFromMasterSeed(),#@	Random number generation seed
		parOmitFlag	 = getEctdColName("ParOmit"),   #@	Parameter omit flag name
		respOmitFlag = getEctdColName("RespOmit"),  #@	Response omit flag name
		missingFlag = getEctdColName("Missing"),   #@	Missing flag name
		interimCol	 = getEctdColName("Interim"),   #@	Interim variable name
		doseCol	     = getEctdColName("Dose"), 	    #@	Dose variable name
		initialDoses = NULL,						#@ Initial set of doses to use in "interim 1"
		stayDropped = TRUE,				#@ Dose dropping flag: if a dose is dropped, should it stay dropped?
		fullAnalysis = TRUE,			#@ Perform a full analysis
		workingPath = getwd(),
		method = getEctdDataMethod()
){
	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# analyzeRep.R Wed Jul 04 12:20:41 BST 2007 @514 /Internet Time/
	#
	# Author: Romain
	###############################################################################
	# DESCRIPTION: wrapper for the analysis step
	# KEYWORDS: component:analysis
	###############################################################################

	# Inner function: Retain values
	"innerRetainValues" <- function(vec) {
		if (all(is.na(vec))) return(rep(0, length(vec)))
		rleNa <- rle(is.na(vec))
		if (rleNa$values[1]) vec[1:(rleNa$lengths[1])] <- 0
		isMiss <- is.na(vec)
		approx(which(!isMiss), vec [ !isMiss ], 1:length(vec), "constant", rule = 2)$y
	}

	# Check replicate number input
	if( !is.numeric(replicate) || length(replicate) != 1 || replicate <= 0 )
		ectdStop("replicate must be a single positive integer")

	## check that the software is SAS or R
	software <- try( match.arg(software), silent = TRUE )
	if (class(software) == "try-error") ectdStop("The software should be `R` or `SAS`")

	# checking the macro code
	if (software == "R") {
		if (class(analysisCode) == "function") analysisCode <- .checkFun( analysisCode, "data" )
		else {
			if (!file.exists(file.path(workingPath, analysisCode))) ectdStop(paste("Cannot find R analysis script file \"", analysisCode, "\"", sep=""))
		}
	}
	else {
		if (!file.exists(file.path(workingPath, analysisCode))) ectdStop(paste("Cannot find SAS analysis script file \"", analysisCode, "\"", sep=""))
	}

	## checks on inputs
	doseCol      <- parseCharInput( doseCol     , expected = 1, valid = TRUE, convertToNumeric = FALSE )

	## import the data
	idata <- readData( dataNumber = replicate, dataType = "Replicate",
			variables = doseCol, workingPath = workingPath, method = method)
	columns <- names( idata )
	doses <- sort( unique( idata[[ doseCol ]]  ) )

	## check the flags
	parOmitFlag	 <- parseCharInput( parOmitFlag	, expected = 1, convertToNumeric = FALSE )
	if (removeParOmit) {
		valid <- try( validNames( parOmitFlag ), silent = TRUE )
		if ( class(valid) == "try-error" ) ectdStop("Invalid format for parameter omit flag variable name")
		if ( !(parOmitFlag %in% columns) ) removeParOmit <- FALSE
	}

	respOmitFlag <- parseCharInput( respOmitFlag, expected = 1, convertToNumeric = FALSE )
	if (removeRespOmit) {
		valid <- try( validNames( respOmitFlag ), silent = TRUE )
		if ( class(valid) == "try-error" ) ectdStop("Invalid format for response omit flag variable name")
		if ( !(respOmitFlag %in% columns) ) removeRespOmit <- FALSE
	}

	missingFlag <- parseCharInput( missingFlag, expected = 1, convertToNumeric = FALSE )
	if (removeMissing) {
		valid <- try( validNames( missingFlag ), silent = TRUE )
		if ( class(valid) == "try-error" ) ectdStop("Invalid format for 'Missing' flag variable name")
		if ( !(missingFlag %in% columns) ) removeMissing <- FALSE
	}

	interimCol   <- parseCharInput( interimCol  , expected = 1, convertToNumeric = FALSE, valid = TRUE )
	valid <- try( validNames( interimCol ), silent = TRUE )
	if ( class(valid) == "try-error" ) ectdStop("Invalid format for interim allocation variable name")
	if (!(interimCol %in% columns) || !is.numeric(idata[[interimCol]]) || any(idata[[interimCol]] < 0) ) interimCode <- NULL

	## check the software
	software <- try( match.arg( software ), silent= TRUE)
	if (class(software) == "try-error") ectdStop("Software should be `R` or `SAS`")

	## subset data according to the remove Flags
	removeSub <- NULL
	if( removeParOmit ) removeSub <- c( removeSub , paste( "( ", parOmitFlag  , " != 1 ) ", sep = "") )
	if( removeMissing ) removeSub <- c( removeSub , paste( "( ", missingFlag , " != 1 ) ", sep = "") )
	if( removeRespOmit) removeSub <- c( removeSub , paste( "( ", respOmitFlag , " != 1 ) ", sep = "") )
	if (length(removeSub)) {
		removeSub <- paste( removeSub , collapse = " & " )
		idata <- idata[ eval( parse( text = removeSub ), idata ), ,drop = FALSE ]
	}

	# Perform full analysis of the data including all the doses found in the data (if required)
	if (fullAnalysis | is.null(interimCode)) {
		.log(" ... full analysis")
		fullOutput <-  performAnalysis( analysisCode = analysisCode, seed = seed,
				data = idata, software = software, doses = doses, doseCol = doseCol,
				workingPath = workingPath  )

		## add more variables to the dataset
		if (is.data.frame(fullOutput) && (nRows <- nrow(fullOutput))) {
			alldata <- data.frame(
				INTERIM = rep(0, nRows),
				INTERIMC = rep("FULL", nRows),
				fullOutput,
				stringsAsFactors = FALSE)
			if (doseCol %in% names(fullOutput)) {
				alldata$INCLUDED = rep(1, nRows)
				alldata$DROPPED = rep(0, nRows)
			}
			alldata$STOPPED <- rep(0, nRows)
		}
		else alldata <- NULL
	}
	else alldata <- NULL

	## cycle through the interims
	if ( !is.null(interimCode) ){

		# check if there is code
		uniqueInterim <- unique( idata [[interimCol]] )
		if (missing(interimCode)) ectdStop("No interim Code found")

		# check if the function exists
		interimCode <- try( match.fun(interimCode), silent =TRUE )
		if (class(interimCode) == "try-error") ectdStop("Cannot find the interimCode function")

		# number of interim
		nInterim <- max( idata[[ interimCol ]])

		includeRows <- dropped <- beenDropped <- NULL
		includeDoses <- if (!is.null(initialDoses)) initialDoses else doses

		for( int in 1:nInterim ){

			.log( " ... interim $int / $nInterim" )

			# make the new subset
			includeRows <- rbind(includeRows, cbind(int, includeDoses))

			# perform the analysis on the interim data
			newAnalysis <- try(
					performAnalysis( analysisCode, seed = seed, data = idata,
							software = software, includeRows = includeRows,
							doses = doses, doseCol = doseCol,
							interimCol = interimCol, workingPath = workingPath ),
					silent = TRUE )
			if (class(newAnalysis) == "try-error") ectdStop("Error when executing `performAnalysis`\n\t$newAnalysis")

			# If anything has come back from the analysis, behave accordingly
			if (is.data.frame(newAnalysis) && (nRows <- nrow(newAnalysis))) {

				# Add interim columns to the data
				newAnalysis <- data.frame(
					INTERIM = rep(int, nRows),
					INTERIMC = if(int == nInterim) rep("FINAL", nRows) else rep(int, nRows),
					newAnalysis,
					stringsAsFactors = FALSE)
				if (doseCol %in% names(newAnalysis)) newAnalysis$INCLUDED <- as.numeric(newAnalysis[[doseCol]] %in% includeDoses)

				# Call the interimAnalysis function to get data changes
				iList <- try(interimAnalysis( newAnalysis, interimCode, uniqueDoses = doses ))
				if (class(iList) == "try-error") ectdStop("Interim analysis step failed")

				# Work out actions based on the return "DROP" element
				dropKeep <- any(c("DROP", "KEEP") %in% names(iList))
				if ( dropKeep ) {
					if ("DROP" %in% names(iList)) {
						# Specifies dose levels to drop as a numeric vector
						dropped <- intersect(iList$DROP, includeDoses)
						if (length(dropped)) includeDoses <- setdiff(includeDoses, dropped)
					}
					else {
						# Logical vector, corresponding to unique doses
						if (length(iList$KEEP) != length(doses)) ectdStop("Logical 'drop doses' return from interim analysis not of correct length")
						whichInclude <- doses[iList$KEEP]
						dropped <- setdiff(includeDoses, whichInclude)
						includeDoses <- whichInclude
					}

					# If dose has been dropped before, don't allow it to be reopened
					if (stayDropped) {
						beenDropped <- union(beenDropped, dropped)
						includeDoses <- setdiff(includeDoses, beenDropped)
					}

				}
				else dropped <- NULL

				# Add columns to the analysis output
				if (doseCol %in% names(newAnalysis)) newAnalysis$DROPPED <- as.numeric(newAnalysis[[doseCol]] %in% dropped)
				newAnalysis$STOPPED = rep(1 * ( "STOP" %in% names(iList) && iList$STOP ), nrow(newAnalysis))

				# Add new analysis to existing data
				alldata <- rbind( alldata, newAnalysis)

				# Do we need to stop the trial?
				if ( "STOP" %in% names(iList) && iList$STOP ) break			# Stop the trial if specified
				if (!length(includeDoses)) break							# Stop the trial if no doses to include in next interim
			}
		}
	}

	if (any(myTest <- names(alldata) == "INTERIM")) names(alldata)[myTest] <- interimCol
	alldata

}

