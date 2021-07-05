#' Add residual error to the response.
#' 
#' Adds residual error to a generated response, based on a supplied
#' variance-covariance matrix
#' 
#' The first step in the algorithm will be to set the random number seed to the
#' "seed" argument.  Then, a number of samples will be drawn from a
#' multivariate normal distribution with mean 0 and covariance set by the
#' (parsed) \code{covariance} input.  The number of samples to take will be set
#' by the number of elements in the "response" vector.
#' 
#' Based on the "errStruc" input, the algorithm should continue as follows.  *
#' If it is Additive, the residual errors should be added to the response
#' vector.  * If it is Log Normal, the response vector is multiplied by the
#' exponentiated residual error.  * If it is Proportional, the response vector
#' is multiplied by "1 + the residual error".
#' 
#' @param response (Required) Numeric vector of response data
#' @param covariance (Required) Residual error (co)variance.  This would be the
#' lower-triangle of the matrix, or the matrix itself. The function
#' \code{\link{parseCovMatrix}} is used to ensure that the matrix is in the
#' right format.
#' @param errStruc (Optional) Function describing how to apply residual error:
#' "Additive", "Log-Normal" or "Proportional".  "Additive" is the default
#' @param seed (Optional) Random seed to use.  Derived from the current random
#' seed by default
#' @return A numeric vector
#' @note In earlier versions of MSToolkit, the "Proportional" Error Structure
#' behaved differently (it assumed parameters passed were on the log scale)
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso This function is typically not directly called by the user, but
#' rather called by the function \code{\link{createResponse}} that is the
#' high-level function for the response component.
#' 
#' The function \code{\link{createResponseVariable}} is also in the response
#' component.  It create the the response onto which \code{addResidualError}
#' adds error.
#' @keywords datagen
#' @examples
#' 
#'   myVec <- 1:10
#'   addResidualError(response = myVec, covariance = "1" )
#' 
"addResidualError" <- function(
	response,                       								#@ numeric vector of response data
	covariance,                     								#@ lower triangle or matrix
	errStruc = c("Additive", "Proportional", "Log-Normal"),			#@ function describing how to apply residual error
	seed = .deriveFromMasterSeed( )									#@ Random Seed to use
) { 
	################################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# addResidualError.R Tue Jun 19 16:17:20 BST 2007 @678 /Internet Time/
	#
	# Author: Romain/Rich P
	################################################################################
	# DESCRIPTION: add residual error to a response
	# KEYWORDS: component:response
	################################################################################

	.requiredArgs(response, "The 'response' variable is required")
	.requiredArgs(covariance, "The 'covariance' argument is required")
	set.seed(seed)

	# <TODO>
	# currently handle only one variable
	covariance <- parseCovMatrix( covariance, 1)                  
	# </TODO>

	errFun <- if(is.function(errStruc)) errStruc 
	else {
		getErrStruc <- try(match.arg(errStruc))
		if (class(getErrStruc) == "try-error") {
			errStruc <- casefold(substring(errStruc, 1, 1), upper = TRUE)
			getErrStruc <- match.arg(errStruc, c("Additive", "Proportional", "Log-Normal"))
		}
		switch( getErrStruc, 
			"Additive"       = function(x,y) x+y,      			# Additive
       		"Log-Normal"     = function(x,y) x * exp(y),  		# Log-Normal
			"Proportional"   = function(x,y) x * (1 + y)  		# Proportional
		)
	}
  
	error <- rnorm( length(response), mean = 0, sd = sqrt(covariance[1,1]) )
  
	if( length(formals(errFun)) < 2  ) ectdStop("The error function should take at least two arguments")
  
	out <- errFun( response, error )
	if( !is.numeric(out)) ectdStop("The error function should return a numeric vector") 
	if( length(out) != length(response)){     
    	ectdStop(
    		"The error function supplied generates a vector that does not have" %.nt%
      		"the same length as the response vector supplied" 
		) 
	}
	out  
}

