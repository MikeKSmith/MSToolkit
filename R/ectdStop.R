#' Error handling functions for the ectd package.
#'
#' These functions are alias to the standard R functions stop and warning, but
#' they also do character interpolation. See examples.
#'
#' @aliases ectdStop ectdWarning
#' @param msg (Required) Message to be displayed to the user
#' @param verbose (Optional) Should more details be given when showing the
#' error message/warning.  By default, this is controlled by support function
#' \link{getEctdVerbose}
#' @inheritParams base::stop
#' @return Nothing.
#' @author Rich Pugh
#' @seealso See \code{\link{warning}} and \code{\link{stop}}.
#' @keywords error
#' @examples
#'
#'
#'   \dontrun{
#'      ectdStop("something wrong happened")
#'   }
#'
#'
"ectdStop" <-
		function (msg, call. = TRUE, domain = NULL, verbose = getEctdVerbose())
{
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# ectdStop.R 02DEC09
	# Author: R.Pugh           
	###############################################################################
	# DESCRIPTION: generate error message
	# KEYWORDS: error, component:support
	###############################################################################

	try(msg <- if (verbose) 
						.strinterp(msg) %.n% "----------------------------------------------------------------------" %.n% 
								paste(sapply(sys.calls(), function(x) as.character(x[[1]])), 
										collapse = " > ") %.n% "----------------------------------------------------------------------"
					else .strinterp(msg), silent = TRUE)
	stop(msg)
}

