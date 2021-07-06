#' Add residual error to the response.
#' 
#' Adds residual error to a generated response, based on a supplied
#' variance values.
#' 
#' The first step in the algorithm will be to set the random number seed to the
#' "seed" argument.  Then, a number of samples will be drawn from a
#' multivariate normal distribution with mean 0 and variance set by the
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
#' behaved differently (it assumed parameters passed were on the log scale).
#'   
#' MSToolkit currently has a limit of two residual error
#' variances / covariances to create additive, proportional or combined 
#' additive + proportional error structures. At this time MSToolkit cannot 
#' simulate from time-series or complex residual variance structures i.e. 
#' multi-variate Normal distributions with number of error terms = number of
#' observations.
#' @author Romain Francois, Rich Pugh
# @seealso This function is typically not directly called by the user, but
# rather called by the function \code{\link{createResponse}} that is the
# high-level function for the response component.
#' 
# The function \code{\link{createResponseVariable}} is also in the response
# component.  It create the the response onto which \code{addResidualError}
# adds error.
#' @keywords datagen
#' @examples
#' 
#'   myVec <- 1:10
#'   addResidualError(response = myVec, covariance = "1" )
#' 
"addResidualError" <- function(
  response,
  covariance,
  errStruc = c("Additive", 
               "Proportional", 
               "Log-Normal",
               "Combined"),
  seed = .deriveFromMasterSeed( )
) { 
  ##############################################################################
  # Mango Solutions, Chippenham SN15 1BN 2009
  # addResidualError.R Tue Jun 19 16:17:20 BST 2007 @678 /Internet Time/
  #
  ##############################################################################
  # DESCRIPTION: add residual error to a response
  # KEYWORDS: component:response
  ##############################################################################
  
  .requiredArgs(response, "The 'response' variable is required")
  .requiredArgs(covariance, "The 'covariance' argument is required")
  set.seed(seed)
  
  covariance <- parseCovMatrix( covariance, nCov = length(covariance) )

  if(nrow(covariance) > 2) ectdStop(
    "MSToolkit cannot currently use more than 2 residual variance terms")
  
  ## TODO
  ## Error handling if errStruc is not one of accepted types should be ectdStop
  
  errFun <- if(is.function(errStruc)) errStruc 
  else {
    getErrStruc <- try(match.arg(errStruc))
    if (class(getErrStruc) == "try-error") {
      errStruc <- casefold(substring(errStruc, 1, 1), upper = TRUE)
      getErrStruc <- match.arg(errStruc, 
                               c("Additive", 
                                 "Proportional", 
                                 "Log-Normal",
                                 "Combined"))
    }
    switch( getErrStruc, 
            "Additive"       = function(x,y) x + y,
            "Log-Normal"     = function(x,y) x * exp(y),
            "Proportional"   = function(x,y) x * (1 + y),
            "Combined"      = function(x,y) x + y[1] + y[2]*x 
    )
  }
  
  error <- MASS::mvrnorm(n = length(response), 
                         mu = rep(0,nrow(covariance)), 
                         Sigma = covariance)
  
  if( length(formals(errFun)) < 2  ) ectdStop(
    "The error function should take at least two arguments")
  
  if( nrow(covariance) > 1){
    if(getErrStruc %in% c("Additive","Proportional", "Log-Normal")) ectdStop(
"Additive, Proportional or Log-Normal errors must use univariate covariance")
  } 
  if( nrow(covariance) != 2 & getErrStruc == "Combined") ectdStop(
      "Combined error structure must use bivariate covariance structure")

  out <- errFun( response, error )
  
  if( !is.numeric(out)) ectdStop(
    "The error function should return a numeric vector") 
  if( length(out) != length(response)){     
    ectdStop(
      "The error function supplied generates a vector that does not have" %.nt%
        "the same length as the response vector supplied" 
    ) 
  }

  out  
}

