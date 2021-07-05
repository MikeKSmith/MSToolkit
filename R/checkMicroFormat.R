#' Check that an input data structure is a valid "Micro Evaluation" structure
#'
#' \code{checkMicroFormat} checks that a given data frame meets certain criteria
#' and returns either a modified version of that data frame or an "empty" micro
#' evaluation frame with given dose data and column names. See the details
#' section below for more information.
#'
#'
#' \code{checkMicroFormat} will perform a series of basic checks on \code{data}.
#'
#' The checks for a valid structure are as follows: (1) \code{data} is a data
#' frame (2) \code{data} has at least 1 row (3) if "mustHaveDose" is TRUE,
#' \code{data} contains a "Dose" column (given by \code{doseCol})
#'
#' If any of these fail it will stop or return a NULL structure
#'
#' @param data (Required) A data frame to check for validity
#' @param doseCol (Optional) The "Dose" variable name within the supplied data.
#'   "DOSE" by default
#' @param mustHaveDose (Optional) Should the "doseCol" variable necessarily be
#'   present?  Default FALSE
#' @return A data frame as described above
#' @author Francisco Gochez
#' @seealso \code{\link{createEmptyMicro}}
#' @keywords datagen
#' @examples
#'
#'
#'   checkMicroFormat(data = data.frame(DOSE = 1:4),
#'   doseCol = "DOSE",
#'   mustHaveDose = TRUE)
#'
#'
"checkMicroFormat" <- function (
  data,
  doseCol = getEctdColName("Dose"),
  mustHaveDose = FALSE
) {
	##############################################################################
	#Mango Solutions, Chippenham SN14 0SQ 2006 checkMicroFormat.R Wed Jun 27
	#14:54:17 BST 2007 Author: Francisco Gochez
	##############################################################################
	#DESCRIPTION: Checks whether or not a given data frame is of the correct
  # format for a micro evaluation data entry
  # KEYWORDS: Documented in Support Functions
	##############################################################################

	# Check data structure
if(!is.data.frame(data)) ectdStop("Output from analysis must be a data frame")
if(!nrow(data)) {
  ectdStop("Output from analysis must be a data frame with at least 1 row")
}

	# Check if "Dose" column exists
	if (mustHaveDose) {
		doseCol <- parseCharInput( doseCol,
		                           convertToNumeric = FALSE,
		                           expected = 1,
		                           valid = TRUE )
		if (!(doseCol %in% names(data))) {
			caseTest <- casefold(names(data)) == casefold(doseCol)
			if (any(caseTest)) names(data)[caseTest] <- doseCol
			else ectdStop("Output from analysis must contain a 'DOSE' column")
		}
	}

	# Return data
	data
}
