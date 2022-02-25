#' Analyze simulated data replicates
#'
#' Analyzes a set of simulated trial data, possibly including interim analyses
#'
#' The first task of the function will be to check the options specifed: * If
#' the "grid" network is unavailable or if the length of the "replicates" input
#' is 1, the "grid" flag will be set to FALSE * If the "grid" flag is TRUE, the
#' call to \code{\link{analyzeData}} will be split across multiple processors
#' using the "parallel" library * If the length of the "replicates" vector is
#' 1, the "waitAndCombine" flag will be set to FALSE * If the "waitAndCombine"
#' flag is set to FALSE, the "cleanUp" flag will also be set to FALSE
#'
#' The \code{\link{analyzeData}} function will iterate around each replicate
#' specified in the "replicates" vector.  For each replicate, the function will
#' first call the \code{\link{analyzeRep}} with the required inputs. The output
#' from the call to \code{\link{analyzeRep}} will be a data frame containing
#' micro evaluation data.  This data frame will be checked to ensure it is of
#' the correct format. If the return from \code{\link{analyzeRep}} is a valid
#' "Micro Evaluation" dataset, it will be saved to the "MicroEvaluation"
#' folder, and also passed to the \code{\link{macroEvaluation}} function for
#' further analysis. If the return from \code{\link{macroEvaluation}} is a
#' valid "Macro Evaluation" dataset, it will be saved to the "MicroEvaluation"
#' folder.
#'
#' If the "waitAndCombine" flag is set to TRUE, the function will wait until
#' all grid jobs are finished (if grid has been used), then compile the "Micro"
#' and "Macro" evaluation results into single summary files (using the
#' \code{\link{compileSummary}} function).
#'
#' @aliases analyseData
#'
#' @param replicates (Optional) Vector of replicates on which to perform
#' analysis: all replicates are analyzed by default
#' @param analysisCode (Required) File containing analysis code (for R or SAS)
#' or an R function for analysis (R only)
#' @param macroCode (Required) An R function to be used for macro evaluation of
#' the result datasets.  See the help file for the
#' \code{\link{macroEvaluation}} function for more information
#' @param interimCode (Optional) An R function to be applied to interim
#' datasets in order to creation interim decisions.  See the help file for the
#' \code{\link{interimAnalysis}} function for more information.  By default, no
#' functions is provided, resulting in no interim analyses being performed
#' @param software (Optional) The software to be used for analysis: either "R"
#' or "SAS".  "R" is the default software used
#' @param grid (Optional) If available, should the analysis be split across
#' available CPUs.  Uses the "parallel" package to split jobs across available
#' cores.  Uses minimum of either: Cores-1 or getOption("max.clusters"),
#' usually =2. FALSE by default.
#' @param waitAndCombine (Optional) Should the process wait for all analyses to
#' finish, then combine into micro and macro summary files?  TRUE by default
#' @param cleanUp (Optional) Should micro/macro directories be removed on
#' completion?  TRUE by default
#' @param removeMissing (Optional) Should rows marked as 'Missing' during the
#' data generation step be removed from the data before analysis is performed?
#' TRUE by default
#' @param removeParOmit (Optional) Should any rows marked as 'Omitted' during
#' the parameter data generation step (ie. parameters out of range) be removed
#' from the data before analysis is performed?  TRUE by default
#' @param removeRespOmit (Optional) Should any rows marked as 'Omitted' during
#' the response generation step (ie. responses out of range) be removed from
#' the data before analysis is performed?  TRUE by default
#' @param seed (Optional) Random number seed to use for the analysis.  Based on
#' the current random seed by default
#' @param parOmitFlag (Optional) Parameter omit flag name.  "PAROMIT" by
#' default
#' @param respOmitFlag (Optional) Response omit flag name.  "RESPOMIT" by
#' default
#' @param missingFlag (Optional) Missing flag name.  "MISSING" by default
#' @param interimCol (Optional) Interim variable name.  "INTERIM" by default
#' @param doseCol (Optional) Dose variable name.  "DOSE" by default
#' @param sleepTime (Optional) Number of seconds to sleep between iterative
#' checks for grid job completion.  15 seconds are used by default
#' @param deleteCurrData (Optional) Should any existing micro evaluation and
#' macro evaluation data be removed before new analysis is performed?  TRUE by
#' default
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
#' @return This function will produce no direct output.  As a consequence,
#' however, many analysis, summary and log files will be produced.
#' @note There are some restrictions on the code inputs to the
#' \code{\link{analyzeData}} function.  These restrictions are discussed here:
#'
#' Analysis Code: The "analysisCode" input must be either an R function or a
#' reference to an external file.  If it is a reference to external file, it
#' must contain either SAS code (if software is "SAS") or R code (if software
#' is "R").  If the code is an R function, or an external R script, it must
#' accept a data frame as it's only argument and return an acceptable "Micro
#' Evaluation" data frame as set out in \code{\link{checkMicroFormat}}.  If the
#' code is an external SAS script, it must accept use a SAS dataset called
#' "work.infile" and create a SAS dataset called "work.outfile" that conforms
#' to the "Micro Evalutation" format as set out in
#' \code{\link{checkMicroFormat}}.  More information on "Micro Evaluation"
#' structures can be found in the help file for function
#' \code{\link{checkMicroFormat}}.
#'
#' Interim Code: The "interimCode" input must be an R function that accepts a
#' single "Micro Evaluation" data input, and returns an R "list" structure that
#' is either empty or contains one or more of the following elements: An
#' element called "STOP" which is a logical vector of length 1.  This tells the
#' \code{\link{analyzeData}} function whether the analysis should be halted at
#' this interim An element called "DROP" which is a vector of numeric values
#' relating to doses in the data to drop before the next interim is analyzed.
#' More information on "Micro Evaluation" structures can be found in the help
#' file for function \code{\link{interimAnalysis}}.
#'
#' Macro Code: The "macroCode" input must be an R function that accepts an
#' enhanced "Micro Evaluation" data input, and returns a valid "Macro
#' Evaluation" data structure (as specified in the help file for the
#' \code{\link{checkMacroFormat}} function.
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso \code{\link{analyzeRep}}, \code{\link{macroEvaluation}},
#' \code{\link{compileSummary}} and \code{\link{generateData}}
#' @keywords datagen
#' @examples
#' \dontrun{
#'
#' # Standard analysis code
#' emaxCode <- function(data){
#'   library(DoseResponse)
#'   with( data,
#'     {
#'     uniDoses <- sort( unique(DOSE))
#'     eFit <- emaxalt( RESP, DOSE )
#'     outDf <- data.frame( DOSE = uniDoses,
#'       MEAN = eFit$dm[as.character(uniDoses)],
#'       SE = eFit$dsd[as.character(uniDoses)] )
#'     outDf$LOWER <- outDf$MEAN - 2 * outDf$SE
#'     outDf$UPPER <- outDf$MEAN + 2 * outDf$SE
#'     outDf$N     <- table(DOSE)[ as.character(uniDoses) ]
#'     outDf
#'   })
#' }
#'
#' # Macro evaluation code
#' macrocode <- function(data) {
#'   # making up a t-test
#'   mu0   <- data$MEAN[ data$DOSE == 0 & data$INTERIM == 0]
#'   mu100 <- data$MEAN[ data$DOSE == 100 & data$INTERIM == 0]
#'   n0    <- data$N[ data$DOSE == 0 & data$INTERIM == 0]
#'   n100  <- data$N[ data$DOSE == 100 & data$INTERIM == 0]
#'   sd0   <- data$SE[ data$DOSE == 0 & data$INTERIM == 0]
#'   sd100 <- data$SE[ data$DOSE == 100 & data$INTERIM == 0]
#'
#'   sddiff <- if( n0 == n100 ){
#'     sqrt( (sd0^2 + sd100^2)  / (n0 + n100) )
#'   } else {
#'     sqrt( (1/n0 + 1/n100) * ( (n0-1)*sd0^2 + (n100-1)*sd100^2  ) / (n0+n100-2)  )
#'   }
#'   tstat  <- ( mu100 - mu0 ) / sddiff
#'   success <- abs(tstat) > qt( .975, n0+n100-2)
#'
#'   data.frame( SUCCESS = success, TSTAT = tstat )
#' }
#'
#' # Interim analysis code
#' interimCode <- function( data ){
#'   dropdose  <- with( data, DOSE [ sign(UPPER) != sign(LOWER) & DOSE != 0] )
#'   outList <- list()
#'   if( length(dropdose) > 0 ) outList$DROP <- dropdose
#'   outList$STOP <- length(dropdose) == nrow(data)-1
#'   outList
#' }
#'
#' # Run analysis
#' analyzeData( 1:5, analysisCode = emaxCode, macroCode = macrocode,
#'   interimCode = interimCode )
#'
#' }
#'
"analyzeData" <- function(
  replicates = "*",                  #@ Replicates to perform analysis on
  analysisCode,                      #@ Function taking a data
  macroCode,                         #@ Macro evaluation code
  interimCode = NULL,                #@ Interim analysis code
  software = "R",                    #@ Software for analysis: R or SAS
  grid = FALSE,                       #@ Split analysis across the grid?
  waitAndCombine = TRUE,             #@ Wait for all analyses to finish, then combine into single file?
  cleanUp = FALSE,                   #@ Delete micro/macro directories on completion?
  removeMissing = TRUE,              #@ Remove Missing rows?
  removeParOmit = TRUE,              #@ Remove Parameter Omit rows?
  removeRespOmit = TRUE,             #@ Remove Response Omit rows?
  seed = .deriveFromMasterSeed(),    #@ Random number seed
  parOmitFlag = getEctdColName("ParOmit"),           #@ Parameter omit flag name
  respOmitFlag = getEctdColName("RespOmit"),         #@ Response omit flag name
  missingFlag = getEctdColName("Missing"),           #@ Missing flag name
  interimCol = getEctdColName("Interim"),            #@ Interim variable name
  doseCol = getEctdColName("Dose"),                  #@ Dose variable name
  sleepTime = 15,                    #@ Number of seconds to sleep between checking for grid jobs
  deleteCurrData = TRUE,             #@ Delete current analysis results before executing
  initialDoses = NULL,					#@ Initial doses to use for "Interim 1"
  stayDropped = TRUE,				#@ Dose dropping flag: if a dose is dropped, should it stay dropped?
  fullAnalysis = TRUE,			#@ Perform a full analysis
  workingPath = getwd(),              #@ Working path containing data
  method = getEctdDataMethod()
)
{
	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# analyzeData.R Tue Jul 03 16:24:00 BST 2007 @447 /Internet Time/
	#
	# Author: Richard, Romain
	###############################################################################
	# DESCRIPTION: High level function to analyze simulated trial datasets
	# KEYWORDS: high, analyze
	###############################################################################
	# TESTME
	funCall <- match.call()

	## Check network connectivity
	macroCode <- .checkFun(macroCode, "data")
	replicates <- .checkReplicates( replicates, workingPath = workingPath, method = method)

	pb <- txtProgressBar(min = 0, max = max(replicates,1), style = 3)

	if (grid && !.checkGridAvailable()) grid <- FALSE
	if (length(replicates) == 1) grid <- waitAndCombine <- FALSE

	## Check directories
	if (deleteCurrData) removeDirectories(c("Micro", "Macro"), workingPath = workingPath)
	createDirectories(c("MicroEvaluation", "MacroEvaluation"), workingPath = workingPath)

	## Split jobs and call grid
	if (grid) {

		nclusters <- parallel:::detectCores() - 1
		if (is.numeric(getOption("max.clusters"))) nclusters <- min(nclusters, getOption("max.clusters"))
		cl <- parallel:::makeCluster(nclusters)
		stopCluster <- parallel:::stopCluster

		repSplit <- .splitGridVector(replicates, ceiling(length(replicates) / nclusters ))

		arguments=list( analysisCode = analysisCode,
						interimCode = interimCode,
                        software = software,
                        removeMissing = removeMissing,
						removeParOmit = removeParOmit,
                        removeRespOmit = removeRespOmit,
						seed = seed,
                        parOmitFlag = parOmitFlag,
                        respOmitFlag = respOmitFlag,
						missingFlag = missingFlag,
                        interimCol = interimCol,
                        doseCol = doseCol,
						initialDoses = initialDoses,
                        stayDropped = stayDropped,
                        fullAnalysis = fullAnalysis,
						workingPath = workingPath,
                        macroCode = macroCode,
                        method = method)

        clusterApply(cl = cl, repSplit, function(l,a){
			for (i in l) {
			  setTxtProgressBar(pb, i)

				microData <- analyzeRep(replicate = i, analysisCode = a$analysisCode,
						interimCode = a$interimCode, software = a$software, removeMissing = a$removeMissing,
						removeParOmit = a$removeParOmit, removeRespOmit = a$removeRespOmit,
						seed = a$seed + i, parOmitFlag = a$parOmitFlag, respOmitFlag = a$respOmitFlag,
						missingFlag = a$missingFlag, interimCol = a$interimCol, doseCol = a$doseCol,
						initialDoses = a$initialDoses, stayDropped = a$stayDropped, fullAnalysis = a$fullAnalysis,
						workingPath = a$workingPath, method = a$method)

				# Write out data
				if (is.data.frame(microData) && nrow(microData)) {

					writeData(microData, i, "Micro", workingPath = a$workingPath)

					macroData <- macroEvaluation(microData, macroCode = a$macroCode,
							interimCol = a$interimCol, doseCol = a$doseCol)

					writeData(macroData, i, "Macro", workingPath = a$workingPath)
				}
				else ectdWarning(paste("No return output from replicate", i))
			}
		}, arguments)

		stopCluster(cl)
		evalTime <- Sys.time()                              # Store time at grid evaluation
	} else {

		# Loop through and analyze replicates

		for (i in replicates) {
		  setTxtProgressBar(pb, i)
			## TODO: Update analyzeRep and performAnalysis with data storage method ..
			microData <- analyzeRep(replicate = i, analysisCode = analysisCode,
				interimCode = interimCode, software = software, removeMissing = removeMissing,
				removeParOmit = removeParOmit, removeRespOmit = removeRespOmit,
				seed = seed + i, parOmitFlag = parOmitFlag, respOmitFlag = respOmitFlag,
	        	missingFlag = missingFlag, interimCol = interimCol, doseCol = doseCol,
				initialDoses = initialDoses, stayDropped = stayDropped, fullAnalysis = fullAnalysis,
				workingPath = workingPath, method = method)

			# Write out data
			if (is.data.frame(microData) && nrow(microData)) {

				writeData(microData, i, "Micro", workingPath = workingPath)

				macroData <- macroEvaluation(microData, macroCode = macroCode,
					interimCol = interimCol, doseCol = doseCol)

				writeData(macroData, i, "Macro", workingPath = workingPath)
			}
			else ectdWarning(paste("No return output from replicate", i))
		}
	}

	if (waitAndCombine) {

    	compileSummary("Micro", workingPath = workingPath)
    	compileSummary("Macro", workingPath = workingPath)

	}
	.cleanup( cleanUp = cleanUp, grid = grid, workingPath = workingPath )

	invisible()
}

analyseData <- analyzeData