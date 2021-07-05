#' check the user supplied drop out function
#' 
#' This function performs checks to ensire that the user-supplied dropout
#' function is correct.
#' 
#' A correct dropout function must have at least a \code{data} argument, and it
#' must return a numeric vector containing the values 0 and 1 having as its
#' length the number of rows of the dataset it is given.
#' 
#' @param fun (Required) Function to check for validity
#' @param data (Required) Dataset to use for performing the valildity check
#' @param sizeSubset (Optional) Number of "initial" rows from the provided data
#' to use in the test for the validity of the function.  The default is "5"
#' @param useSubset (Optional) Should the check operate on a subset of the
#' data, as opposed to the entire dataset?  TRUE by default
#' @param \dots (Optional) Extra arguments to be passed directly to the
#' function being tested.  No extra arguments are passed by default
#' @return Nothing. Function only used for the sied effect of generating an
#' error if the function is not correct.
#' @note if \code{useSubset} is set to TRUE, only a subset of the data is used
#' to perform the check. The size of the subset is then the minimum of the
#' \code{sizeSubset} and the number of rows in the dataset.
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso \code{\link{createDropout}} calls this function before creating the
#' drop out flag.
#' @keywords error
#' @examples
#' 
#'   dFun <- function(data, prop) sample(0:1, nrow(data), TRUE, c(1-prop, prop))
#'    testData <- data.frame(
#'      SUBJ=rep(1:10, each=5), 
#'      TIME=rep(0:4, 10), 
#'      VALUE=rnorm(50))
#'   checkDropOutFun( dFun, testData, prop = .2 )
#'   
#'   \dontrun{
#'     # wrong function
#'     checkDropOutFun( max, testData )
#'     
#'     # function that does not exist
#'     checkDropOutFun( "XXXX", testData )
#'     
#'     # function that does not exist
#'     checkDropOutFun( XXXX, testData )
#'     
#'   }
#' 
#' 
"checkDropOutFun" <- function(
	fun,          			#@ dropout function to check
	data,         			#@ Data on which to execute function
	sizeSubset = 5, 		#@ size of the subset to use to perform the check
	useSubset = TRUE, 		#@ Logical flag, do we subset before performing the check
	...           			#@ Extra arguments to pass to the function
){
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# checkDropOutFun.R Tue Jun 19 11:12:38 BST 2007 @467 /Internet Time/
	#     
	# Author: Romain/Rich P
	###############################################################################
	# DESCRIPTION: check the validity of the drop out function
	# KEYWORDS: component:support
	###############################################################################
	
	## make sure it is a function
	fun <- try( match.fun(fun), silent = TRUE )
	if (class(fun) == "try-error") ectdStop("Dropout function could not befound")
	if (!is.function(fun)) ectdStop("Dropout function is not a function")

	# Check for a data argument
	nf <- names( formals( fun ) )  
	if (!any(nf == "data")) ectdStop("The drop out function must have a `data` argument")
  
	# Run function on section of data
	hd <- if( useSubset ) head( data, n = sizeSubset  ) else data
	out <- try( fun( hd, ... ) , silent = TRUE) 
	if (class(out) == "try-error") ectdStop("Error when calling the dropout function on a subset of data")

	# Check the output from the function
	if (length(out) != nrow(hd )) ectdStop("The Dropout function outputs a vector of wrong length")
	if (!all(unique( as.integer( out )) %in% 0:1)) ectdStop("The dropout function outputs a vector with values different from 0 and 1")

	# Return TRUE flag if we've got this far
	invisible( TRUE )
}
