#' Check the macro format dataset
#'
#' This function is used to perform checks on the structure of the macro format
#' dataset produced by the user-supplied macro code in
# \code{\link{analyzeData}}.
#'
# The \code{\link{checkMacroFormat}} function checks that the data passed to
# it is a valid "Macro Evaluation" structure.  A valid "Macro Evaluation"
# structure is an R data frame containing only 1 row of data.
#'
#' @param data (Required) Dataset to be checked to ensure it is a valid "Macro
#' Evaluation" structure
#' @return Nothing. Only used for error generation side effects.
#' @author: Francisco Gochez, Rich Pugh
# @seealso \code{\link{analyzeData}}
#' @keywords IO
#' @examples
#'
#'   checkMacroFormat(iris[1,])
#'
#'   \dontrun{
#'     # not a data frame
#'     checkMacroFormat( 1:10 )
#'
#'     # more than one row
#'     checkMacroFormat(iris[1:2,])
#'
#'   }
#'
#' @export
"checkMacroFormat" <- function(
	data    #@ Data to check
)
{
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# Fri Jun 21 16:51 BST 2007 @445 /Internet Time/
	#
	# Author: Francisco Gochez / Rich Pugh
	###############################################################################
	# DESCRIPTION: Checks whether a data frame conforms with the expected
	# "Macro Evaluation" data format
	# KEYWORDS:misc, IO
	###############################################################################
	if(!is.data.frame(data) || nrow(data) != 1) ectdStop("Macro evaluation data must be a data frame with a single row")
	invisible(TRUE)
}
