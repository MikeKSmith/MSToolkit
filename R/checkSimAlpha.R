#' Parse alpha value
#'
#' Parses an input alpha value to be used in numeric summary calculations
#'
#' The function first checks that there is only 1 input.  If character, the
#' alpha is parsed and converted to a single numeric. If alpha is greater than
#' 1, it is assumed to be on the `[1,100]` scale, and will be scaled onto the
#' `[0,1]` scale If alpha is less than 0.5, it is assumed to tbe on the `[0,.5]`
#' scale (ie. .05 instead of .95) and converted to the `[.5, 1]` scale
#'
#' @param alpha Alpha value to be parsed
#' @return A single numeric between .5 and 1
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @keywords statistics
#' @examples
#'
#' 	checkSimAlpha("95%")
#' 	checkSimAlpha(95)
#' 	checkSimAlpha(5)
#'
#' @export
"checkSimAlpha" <- function(alpha = 95)
{
	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	#
	# Author: Rich
	###############################################################################
	# DESCRIPTION: Utility function to parse "alpha" input
	###############################################################################
	if(length(alpha) > 1) ectdStop("Only 1 alpha should be provided")
	if(is.character(alpha)) {
		alpha <- gsub(" ", "", alpha)
		suppressWarnings(alpha <- as.numeric(gsub("%", "", alpha)))
		if(any(is.na(alpha))) ectdStop("Could not parse specified alpha")
	}
	if(alpha > 1) alpha <- alpha/100
	if(alpha < 0.5) alpha <- 1 - alpha
	max(min(alpha, 1), 0)
}
