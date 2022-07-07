#' Create a response from a data set and an equation
#'
#' Create a response from a data set and an equation
#'
#' Using the \code{preTest} will make the function fail early if the code is
#' wrong.  This is typically useful during the first steps of try and error.
#' When the user is confident that the \code{equation} is correct, the
#' \code{preTest} can be set to \code{FALSE}.
#'
#' @param data (Required) The dataset to use, must be a
#' \code{\link{data.frame}}
#' @param equation (Required) R function that must have a \code{data} argument
#' or character string describing the equation that uses names of the variables
#' in the data.
#' @param preTest (Optional) Logical Flag. TRUE to try and build the response
#' first with a subset of the data. The subset size is given by the minimum
#' between the number of rows in the data and the \code{subsetSize} argument.
#' @param subsetSize (Optional) Size of the subset if the \code{preTest} has
#' been requested.
#' @return A numeric vector corresponding to the response variable.
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso \code{\link{addResidualError}} adds a residual error to a response.
#'
#' The function \code{\link{createResponse} } is the high level function in the
#' response component and acts as a wrapper for \code{createResponseVariable}
#' and \code{\link{addResidualError}}.
#' @keywords datagen
#' @examples
#'
#'   # define a data set
#'   myData <- data.frame(X = c(1,2,1), Y = c(1,1,1), Z = c(1,1,1))
#'
#'   # added to comply with SF issue 7
#'   # Tue Jul 24 10:20:20 BST 2007 @430 /Internet Time/
#'   # function version
#'   out1 <- createResponseVariable(data = myData, equation = function(data){
#'     with(data, X + Y + Z)
#'   })
#'
#'   # same using the character version
#'   out2 <- createResponseVariable(data = myData, equation = "X+Y+Z")
#'   stopifnot(identical(out1,out2))
#'
#' @export
"createResponseVariable" <- function(
	data,           #@ data frame to whoch to add the response variable
	equation,       #@ function for creting the response variable or text character
	preTest = TRUE, #@ run the pre test ?
	subsetSize = 5  #@ size used for the subset
 ) {
	################################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# createResponseVariable.R Tue Jun 19 15:20:10 BST 2007 @639 /Internet Time/
	#
	# Author: Romain
	################################################################################
	# DESCRIPTION: create the response
	# KEYWORDS: component:response
	################################################################################

	.requiredArgs(data)
	if (!is.data.frame(data)) ectdStop("`data` must be a data frame")
	nr <- nrow( data )

	eqFun <- try( match.fun( equation ), silent = TRUE )
	eqFun <- if( class(eqFun) == "try-error") {
		function( data ){
    		eval( parse( text = equation) ,data )
		}
	} else .checkFun( eqFun, "data")

	### run a pre test to find out if the equation will run a
	### small subset of the data
	if( preTest && nr > subsetSize ){
		subsetData <- data[ 1:subsetSize , , drop=FALSE ]
		out <- try( eqFun( subsetData), silent = TRUE )
		if( class(out) == "try-error" ) ectdStop("Error when evaluating equation on subset of the data")
		if( length(out) != subsetSize ){  #: testLength
			ectdStop(
				"The equation given for the response does not generate" %.nt%
				"a vector of length equal to the dimension of the data"
			)
		}
	}

	### call the equation on the data
	out <- try( eqFun(data), silent  = TRUE )
	if( class(out) == "try-error") ectdStop( "Error when executing the equation statement on the data")

	if( length(out) != nr ){
    	ectdStop(
   			"The equation given for the response does not generate" %.n%
			"a vector of length equal to the dimension of the data ($nr)"
		)
	}
	out
}

