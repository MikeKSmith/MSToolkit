#' Macro Evaluation
#' 
#' The macro evaluation gives a short summary of all analysis performed for a
#' single replicate of data.
#' 
#' The macro code is first studied to make sure that it is a function that
#' takes a \code{data} argument.  The code is then executed against the micro
#' data, and should produce a data frame containing a single row. This is
#' further checked using the \code{\link{checkMacroFormat}} function.
#' 
#' @param data (Required) Data set to use, typically returned from a micro
#' evaluation step.
#' @param macroCode (Required) Function used to summarize the micro data.  If
#' the function has arguments "doseCol" and/or "interimCol", the arguments to
#' macroEvaluation are passed to it in addition to the data
#' @param interimCol (Optional) Name of the interim column, should be a valid
#' name ("INTERIM" by default). See \code{\link{validNames}}
#' @param doseCol (Optional) Name of the dose column, should be a valid name
#' ("DOSE" by default). See \code{\link{validNames}}
#' @return A data frame that complies with \code{\link{checkMacroFormat}}
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @keywords datagen
#' @examples
#' 
#'   # example of micro data with interim
#'   microData <- read.csv( 
#'     system.file( "Runit", "data", "macroEvaluation", "micro0001.csv" , package = "MSToolkit") ) 
#'   mCode <- function(data) {
#'     diffMeans <- data$MEAN[ data$DOSE == 100 & data$INTERIM == 0] - 
#'     data$MEAN[ data$DOSE == 0 & data$INTERIM == 0  ]
#'     data.frame( SUCCESS = diffMeans > 10, NFINAL = sum(data$N) )
#'   }
#'   out <- macroEvaluation( microData, mCode)
#'   stopifnot( nrow(out) == 1 )
#'     
#' 
"macroEvaluation" <- function(
	data,    		               				#@ dataset
	macroCode,      				        	#@ macro code
	interimCol = getEctdColName("Interim"), 	#@ name of the INTERIM column 
	doseCol = getEctdColName("Dose")        	#@ name of the DOSE column
)
{
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# macroEvaluation.R Wed Jun 27 14:32:46 BST 2007 @606 /Internet Time/
	#
	# Author: Romain/Rich P 
	###############################################################################
	# DESCRIPTION: summarise a single set of 
	# KEYWORDS: component:analysis
	###############################################################################
	.log( "Calling macro evaluation function" )

	# Checks on inputs
	if(!is.data.frame(data) || !nrow(data)) ectdStop("Input data must be a data frame with at least 1 row")
	.checkCharacter(interimCol, doseCol)					# Check (character) inputs
	if (!is.function(macroCode)) ectdStop("Macro evaluation code must be a function")

	# Prepare the arguments for the macro evaluation call
	funArgs <- names(formals(macroCode))
	callList <- list(data)
	if ("doseCol" %in% funArgs) {
		checkColNames(names(data), doseCol)
		callList$doseCol <- doseCol
	}
	if ("interimCol" %in% funArgs) {
		checkColNames(names(data), interimCol)
		callList$interimCol <- interimCol
	}
	# Try to run the code on the data
	out <- try( do.call(macroCode, callList), silent = TRUE)
	if(class(out) == "try-error") ectdStop("Error when calling the macroCode \n\t$out")
	
	# Check structure and return
	.log("Checking macro evaluation data")
	checkMacroFormat(out)
	out  
}
