#' Create a SAS 'IF' statement
#'
#' Create a SAS 'IF' statement, used to describe rows to include in a SAS
#' analysis
#'
#' Takes a matrix of interim and dose values, and produces a "SAS style" IF
#' statement
#'
#' @param includeRows Row inclusion matrix (2 columns: Interim value and Dose
#' value)
#' @param doseCol Dose column name, given by \link{getEctdColName} by default
#' @param interimCol Interim column name, given by \link{getEctdColName} by
#' default
#' @return A single character string
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @keywords SAS
#' @examples
#'
#' convertSASIncludeRows( cbind(rep(1:2, each = 3), c(0, 15, 30, 0, 30, 45) ) )
#'
#' @export
"convertSASIncludeRows" <- function(
		includeRows = NULL,    #@ matrix of 2 columns describing the changes to make to the data
		doseCol = getEctdColName("Dose"),
		interimCol = getEctdColName("Interim")
){
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# convertSASIncludeRows.R 15DEC09
	#
	# Author: Rich
	###############################################################################
	# DESCRIPTION: Write "drop doses" code in "SAS" format
	# KEYWORDS: component:analysis
	###############################################################################

	# Parse inputs
	if (!length(includeRows)) return("**** No doses to drop ****;")
	if (!is.matrix(includeRows) || ncol(includeRows) != 2) ectdStop("'includeRows' input should be a matrix with 2 columns")

	# Inner paste subset function
	"innerPasteSASSubset" <- function(vec, interimCol, doseCol) {
		paste("(", interimCol, "=", vec[1], "and", doseCol, "=", vec[2], ")")
	}

	# Perform parsing
	outSub <- apply(includeRows, 1, innerPasteSASSubset, interimCol = interimCol, doseCol = doseCol)
	outSub <- paste(outSub, collapse = " or ")
	paste("IF", outSub, ";")

}
