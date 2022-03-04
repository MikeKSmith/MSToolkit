#' High-level function for the response component.
#'
#' Create a response from a dataset and an equation.
#'
#'
#' @param data (Required) Data structure to which to add response.
#' @param equation (Required) R function or character string for creating
#' response. See \code{\link{createResponseVariable}}.
#' @param name (Optional) Response variable name ("RESP" by default)
#' @param invLink (Optional) Inverse link function for the predictor. This is
#' an R \code{\link{function}}.  No inverse link function by default
#' @param distribution (Optional) Outcome variable distribution, one of "n" for
#' normal, "l" for lognormal, "b" for binomial or "p" for poisson.  Default is
#' "Normal"
#' @param covariance (Optional) Residual Error (CO)VARIANCE, passed to the
#' \code{\link{addResidualError}} function.  Missing by default, resulting in
#' no residual error generated for the reponse
#' @param errStruc (Optional) Function describing how to apply residual error:
#' "Additive", "Log-Normal" or "Proportional".  "Additive" is the default
#' @param range (Optional) Range of acceptable values for created response. See
#' \code{\link{parseRangeCode}}.  Missing by default, resulting in no "range"
#' limitation on the response
#' @param digits (Optional) Number of digits to which to round the response (3
#' by default)
#' @param seed (Optional) Random number seed.  By default, this is derived from
#' the current random seed
#' @param flagName (Optional) "Response Omit" Flag name ("RESPOMIT" by default)
#' @return A data set containing two columns.  \item{RESP}{The actual Response}
#' \item{RESPOMIT}{The response OMIT flag. It is named from the value of the
#' \code{flagName} argument.}
#' @note In earlier versions of MSToolkit, the "Proportional" Error Structure
#' behaved differently (it is assumed parameters passed were on the log scale)
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso The lower-level function of the response component:
#' \code{\link{createResponseVariable}} and \code{\link{addResidualError}} are
#' called by \code{createResponse}.
#' @keywords datagen
#' @examples
#'
#'   # example data
#'   myData <- expand.grid(X = 1:2, Y = 1:2, Z = 1:2)
#'
#'   # added to comply with SF issue 7
#'   # Tue Jul 24 10:21:14 BST 2007 @431 /Internet Time/
#'   # function example
#'   out1 <- createResponse(data = myData, equation = function(data) with(data, X+Y+Z),
#'                          covariance = 1, range = "RESP < 3", seed = 96)
#'
#'   # same example using a character representation
#'   out2 <- createResponse(data = myData, equation = "X+Y+Z",
#'                          covariance = 1, range = "RESP < 3", seed = 96)
#'   stopifnot(identical(out1, out2))
#'
"createResponse" <- function(
	data,          #@ data structure to which
	equation,      #@ function for creating response     >> createResponseVariable
	name = getEctdColName("Response"), #@ Response variable name
	invLink,       #@ inverse link function for predictor
	distribution = "normal",  #@ Outcome variable distribution
	covariance,    #@ Residual error (co)variance
	errStruc = c("Additive", "Proportional", "Log-Normal"),			#@ function describing how to apply residual error
	range,         #@ Range of Acceptable values for created response
	digits = 3,    #@ Number of digits to which round the response
	seed = .deriveFromMasterSeed(),
	flagName = getEctdColName("RespOmit")
) {
	################################################################################
  	# Mango Solutions, Chippenham SN15 1BN 2009
	# createResponse.R Wed Jun 20 09:10:48 BST 2007 @382 /Internet Time/
	#
	# Author: Romain
	###############################################################################
	# DESCRIPTION: create response, wrapper function
	# KEYWORDS: datagen, component:data:response
	##############################################################################

	# Set random number seed
	set.seed(seed)

	# Initial tests on variable names
	validNames( flagName, name)
	if( flagName == name ) ectdStop("`flagName` and `name` should be different")

	# Check digits
	digits <- parseCharInput( digits, expected = 1, msg = "digits should be only one number" )
  	if( digits < 0  ) ectdStop( "`digits` must be a positive integer value, is now : $digits" )

	# Check distribution
	distribution <- initialChar( distribution, "nlbp", "distribution must be either `Normal`, `LogNormal`, `Binomial` or `Poisson`")

	# Handle the invLink
	if ( missing(invLink)) {
		# Use the defaults
    	invLink <- switch( distribution,  "n" = NULL,  "l" = exp,  "b" = plogis, "p" = exp)
	}
	else {
  		# Is the function available ?
    	if( is.character( invLink ) ) {
    		invLink <- try( match.fun(invLink), silent = TRUE )
			if( class(invLink) == "try-error") ectdStop( "The `invLink` function is not available to the system" )
		}
    	# Does it work correctly ?
    	testInvLink <- try( invLink( rep(1, 5) ), silent = TRUE )
    	if( class(testInvLink) == "try-error" ) ectdStop("Errors when calling the invLink function")
		if( length(testInvLink) != 5 ) ectdStop("The `invLink` function does not output a vector of same length as its inputs")
	}

	## Create the response variable using the "createResponseVariable" function
	name %<-% createResponseVariable( data, equation )

	## Add residual Error
	# i need to use get because the user might use the `name` in the
	# `range` code and s?he can give the `name` s?he wants
	if( !missing(covariance) ) name %<-% addResidualError( response = get(name),
				covariance = covariance, errStruc = errStruc, seed = seed )

	## Apply the "inverselink" function
	if ( !is.null(invLink) ){
 		sumNa <- sum(is.na(get(name)))
    	name %<-% suppressWarnings(invLink(get(name)))
    	naNow <- sum(is.na(get(name)))
    	if (naNow > sumNa) ectdStop(paste("Applying inverse link function generated", naNow - sumNa, "missing values in the data"))
	}

	## for lognormal and normal: do nothing, otherwise :
	if( distribution == 'b' ) {           # binomial
		# check if the probabilities are \in [0,1]
		probs <- get(name)
    	if( any(probs < 0 || probs > 1)) ectdStop( "the probabililties are not between 0 and 1")
    	name %<-% rbinom( nrow(data), prob = probs, size = 1 )
	}
	else {
		if( distribution == 'p' ) {    # poisson
    		lambda <- get(name)
    		negTest <- lambda < 0
    		if( any(negTest)) {
      			pNeg <- round(100 * sum(negTest)/length(lambda))
      			ectdWarning(paste(pNeg, "% of lambda values are less than 0 or missing - setting these values to 0 for the poission distribution draw", sep=""))
      			lambda[negTest] <- 0
			}
			name %<-% rpois( nrow(data), lambda = lambda )
		}
	}

	## find out which to flag as omit
	if ( missing(range)) omit <- rep( 0, nrow(data) )
	else {
		rangeExp <- parseRangeCode ( range )
    	omit <- 1 * !eval( rangeExp , data )
    	if( !all(omit %in% c(0,1)) ) ectdStop( "The range code does not produce only 0 and 1 or TRUE and FALSE" )
	}

	## round the response
  	name %<-% round( get(name), digits = digits )

	## make the output data frame
	.eval( "data.frame( $name = get(name), $flagName = omit )" )
}

