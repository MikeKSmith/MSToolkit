#' Create an empty micro evaluation data frame
#'
#' Creates an "empty" (i.e. filled in with NA values) micro evaluation data
#' frame that has its column names and dose column name given by arguments.
#'
#' createEmptyMicro will return a data frame whose column names are given by
#' \code{microColumnNames}. The first column of the resulting data frame will
#' contain the \code{doses} vector by default.  The data frame will have as
#' many rows as there are entries in \code{doses}.  All of the frame's other
#' entries will be NA. This function will generate an error if "doses" contains
#' duplicate entries or if the dose column name specified by \code{doseCol}
#' doeses not occur in \code{microColumnNames}.
#'
#' @param doses (Required) A mixed numeric or character vector of dose data
#' that will be the dose column of the data frame.  It must not contain any
#' duplicate entries.
#' @param doseCol (Optional) A string indicating the name of the column which
#' will hold the " doses" vector.  "DOSE" by default
#' @param microColumnNames (Optional) A character vector containing the names
#' of the columns of the micro evaluation data frame.  By default, variables
#' "MEAN", "SE", "LOWER", "UPPER" and "N" are created in addition to the dose
#' variable
#' @return createEmptyMicro returns a data frame that is as described above.
#' @note Future versions of MSToolkit will relax the format of the
#' MicroEvaluation dataset allowing a more free structure for this dataset.
#' @author Francisco Gochez
#' @seealso \code{\link{checkMicroFormat}}
#' @keywords data
#' @examples
#'
#'   createEmptyMicro(doses = c(10, 20 ,30, 40))
#'   createEmptyMicro(doses = c(5, 25), doseCol = "D", microColumnNames = c("D", "MEAN", "SE",
#'   "LOWER"))
#'
#' @export
createEmptyMicro <- function(
  doses, # A numeric vector containing the DOSES column of the micro data
  doseCol = "DOSE", # The name of the dose column in the data frame
  microColumnNames =  c("doseName" = doseCol, "MEAN", "SE", "LOWER", "UPPER", "N")  # The name of the data frame's columns
)
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # createEmptyMicro.R Wed Jun 27 14:31:26 BST 2007 @445 /Internet Time/
  # Author: Francisco Gochez
  ###############################################################################
  # DESCRIPTION:  Creates a micro evaluation dataframe whose DOSE column is given by an argument and which
  # has the value NA for the rest of its entries.
  # KEYWORDS:
  # Documented in Support Functions Design Specification
  ###############################################################################
{
  doses <- parseCharInput( doses )
  if( length(doses) == 0 || is.null(doses) )
    ectdStop("Empty or null doses vector")

  # Check for duplicate doses and disallow
  if(any(duplicated(doses)))
    ectdStop("duplicated doses in argument")

  # The names of the columns must include the string "doseCol"
  if(doseCol %!in% microColumnNames)
  	ectdStop(paste(doseCol, "does not occur in the microColumnNames"))

  result <- as.data.frame(matrix(nrow = length(doses), ncol = length(microColumnNames)))
  names(result) <- microColumnNames
  result[doseCol] <- doses
  result
}