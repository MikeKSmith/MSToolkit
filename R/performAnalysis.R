#' Runs the analysis code against one dataset
#'
#' That function is iteratively called from \code{\link{analyzeRep}} to analyze
#' a replicate dataset at each step defined by the interims.
#'
#' The function tries to perform the analysis contained in the
#' \code{analysisCode} against the \code{data}.  The analysis code specified is
#' either an external file containing code (SAS and R), or an R function (R
#' only).
#'
#' If software is set to "SAS" the analysis code is assumed to be a reference
#' to an external SAS script.  The SAS code must accept a single dataset called
#' work.infile, and create an output dataset called work.outfile.  The
#' work.outfile dataset must be a valid "Micro Evaluation" structure as
#' specified in the help file for \code{\link{checkMicroFormat}}.  If the
#' software is "R", the analysis code input must be either an R function or an
#' R script.  The R analysis code must also return a valid "Micro Evaluatoin"
#' structure as specified in function \code{\link{checkMicroFormat}}
#'
#' @param analysisCode (Required) Analysis code: An R function, a reference to
#' an external R script, or a reference to an external SAS script
#' @param doses (Required) Vector of doses for which estimates are expected
#' @param data (Required) Input dataset on which to perform analysis
#' @param software (Optional) Software for analysis: "R" or "SAS" (default is
#' "R")
#' @param includeRows (Optional) 2 column matrix specifying interims and doses
#' to include in analysis.  By default, no subsets are applied
#' @param interimCol Interim flag column name
#' @param doseCol Dose column name
#' @param seed (Optional) Random number generation seed.  By default, this is
#' derived from the current random seed
#' @param workingPath (Optional) Working directory for the analysis.  The
#' default is the current working directory
#' @param cleanUp (Optional) Logical: Should we remove the files that have been
#' passed in and out of SAS?
#' @param tempSasDir Temporary Sas Directory
#' @return The function should output a valid "Micro Evaluation" data structure
#' as specified in the \code{\link{checkMicroFormat}} help file.
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso \code{\link{checkMicroFormat}}
#' @keywords models
#' @examples
#' \dontrun{
#'
#'   # Emax example
#'   anaCode <- function(data){
#'     with( data, {
#'       uniDoses <- sort( unique(DOSE))
#'       outDf <- data.frame( DOSE = uniDoses,
#'         MEAN = tapply(RESP, DOSE, mean) ,
#'         SE   = tapply(RESP, DOSE, sd )  )
#'       outDf$LOWER <- outDf$MEAN - 2 * outDf$SE
#'       outDf$UPPER <- outDf$MEAN + 2 * outDf$SE
#'       outDf$N     <- table(DOSE)[ as.character(uniDoses) ]
#'       outDf
#'     })
#'   }
#'
#'   # example data
#'   exData <- system.file( "Runit", "data", "analyseRep", "ReplicateData", "replicate0001.csv",
#'   package = "MSToolkit")
#'   out <- performAnalysis(anaCode, data = exData, doses = c(0, 5, 25, 50, 100) )
#'   checkMicroFormat( out )       # Check the format of the return structure
#'
#' }
#'
#' @export
"performAnalysis" <- function(
  analysisCode,  							#@ File containing the actual analysis code to run on the data
  doses,         							#@ Doses for which estimates are expected
  data,          							#@ Input dataset
  software = c("R","SAS"),      			#@ Software for analysis: R or SAS
  includeRows = NULL,   					#@ Changes to be made to the data before analysis
  interimCol = getEctdColName("Interim"), 	#@ Interim variable name
  doseCol = getEctdColName("Dose"), 	    #@ Dose variable name
  seed = .deriveFromMasterSeed(), #@ Random number generation seed
  workingPath = getwd(),
  cleanUp = TRUE,
  tempSasDir = tempdir()
)
{
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # performAnalysis.R Wed Jun 27 11:08:20 BST 2007 @464 /Internet Time/
  #
  # Author: Romain
  ###############################################################################
  # DESCRIPTION: analyze a single replicate of data
  # KEYWORDS: component:analysis
  ###############################################################################

  ## check that the software
  software <- try( match.arg(software), silent = TRUE )
  if (class(software) == "try-error") ectdStop("The software should be `R` or `SAS`")

  switch( software,
    "SAS" = {

		# Export the data to a file for SAS to use
		infile <- file.path(tempSasDir, "sasDataInput.csv")
		outfile <- file.path(tempSasDir, "sasDataOutput.csv")
		utils::write.csv(data, file = infile)			# Export data so SAS can read it
		if (!file.exists(infile)) ectdStop("Cannot write replicate to an external CSV file")

		# Set up parameters for the SAS call
		sasChanges <- convertSASIncludeRows(includeRows, doseCol = doseCol, interimCol = interimCol) 	# SAS 'keep rows' string
		sasParameters <- paste(infile, outfile, file.path(workingPath, analysisCode), sasChanges, seed, sep="#")
		.log(paste("Calling SAS with execution string \"", sasParameters, "\"", sep=""))

		# Try to call SAS
		trySas <- .ectdSasCall(sasParameters)
		if (class(trySas) == "try-error") ectdStop("Problems occurred when calling SAS in batch mode (see SAS log file for more details)")

		# Has the call been successful?
		if (file.exists(outfile)) {
			sasData <- try(utils::read.csv(outfile))
			if (class(sasData) == "try-error") ectdStop("Could not import SAS analysis output")
			sasData <- checkMicroFormat( sasData, doseCol = doseCol )
         	if (cleanUp) {
				try(file.remove(outfile))
		 		try(file.remove(infile))
			}
		}
		else sasData <- NULL

		return(sasData)
    },
    "R" = {
		# Set the seed
		set.seed( seed )

		# Get vector of doses for which statistics are expected
		if (missing(doses)) doses <- unique( data[[doseCol]] )

      	## apply the includeRows change if needed
      	if (!is.null(includeRows)) data <- .keepAnalysisRows(data, includeRows, interimCol, doseCol)

		## run the analysis code
		if (class(analysisCode) == "function") analysisOutput <- try( analysisCode(data)  , silent = TRUE )
		else {
			if (length(analysisCode) == 1 && file.exists(file.path(workingPath, analysisCode))) { # Script
				analysisOutput <- try(source(file.path(workingPath, analysisCode), local = TRUE)$value, silent = TRUE)
				if (class(analysisOutput) == "try-error") ectdStop(paste("Could not perform analysis using script", file.path(workingPath, analysisCode)))
			}
			else { 	# Character parse
				analysisOutput <- try(eval(parse(analysisCode)), silent = TRUE)
			}
		}
		out <- if( class(analysisOutput) == "try-error" ) {
			ectdWarning("Error when executing analysis code: " %.nt%
				( analysisOutput %-~% "^[^:]*:") %.nt%    # extract the message from the `try`
				"... creating an empty summary file" )

			NULL
      	}
		else {
        	checkMicroFormat( analysisOutput , doseCol = doseCol )
    	}

      	return( out )
    })

}

".keepAnalysisRows" <- function(data, includeRows, interimCol, doseCol) {
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	#
	# Author: Rich P
	###############################################################################
	# DESCRIPTION: Subsets data based on 2 column matrix
	###############################################################################

	# Check the input structure
	if (!is.matrix(includeRows) || ncol(includeRows) != 2) ectdStop("'includeRows' input should be a matrix with 2 columns")

	# Function for pasting subsets
	"innerPasteSubset" <- function(vec, interimCol, doseCol) paste("(", interimCol, "==", vec[1], "&", doseCol, "==", vec[2], ")")

	# Perform the subset creation
	outSub <- apply(includeRows, 1, innerPasteSubset, interimCol = interimCol, doseCol = doseCol)
	outSub <- paste(outSub, collapse = " | ")

	# Try to apply the subset
	myTest <- try(with(data, eval(parse(text = outSub))))
	if (class(myTest) == "try-error" || !is.logical(myTest)) ectdStop("Could not execute subset statement on data")
	if (!any(myTest)) ectdStop("No data to analyze following dose dropping")
	data [ myTest, , drop = FALSE ]
}
