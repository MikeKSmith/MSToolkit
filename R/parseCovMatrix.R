#' Parse input to construct a covariance matrix
#'
#' Creates a symmetric positive definite matrix from a vector if possible, or
#' checks if a given matrix is symmetric positive definite.
#'
#' parseCovMatrix attempts to create a symmetric positive definite matrix of
#' dimension nCov x nCov. If values is a matrix, parseCovMatrix will simply
#' check that it is symmetric and positive definite up to a tolerance as
#' inidicated by tol. If it is a numeric vector, parseCovMatrix will try to
#' create an nCov x nCov positive definite symmetric matrix, with the method of
#' creation depending on values and nCov.  If values contains a single entry,
#' an nCov x nCov diagonal matrix with that single value repeated nCov times
#' will be created.  If the number of entries in "values" is equal to "nCov",
#' parseCovMatrix will create a diagonal matrix whose diagonal will be equal to
#' "values".  if the number of entries in "values" is equal to nCov * (nCov+1)
#' / 2, parseCovMatrix will create a positive definite symmetric matrix with
#' the entries for the lower triangle taken from "values".  If none of these
#' conditions hold or if the entries of "values" are not compatible with
#' positive definite symmetric matrices, an error will be printed.
#'
#' @param values (Required) Either a matrix or a mixed character-numeric vector
#' @param nCov (Required) The number of rows and columns that should be
#' available in the resulting matrix
#' @param tol (Optional) Numerical tolerance for the positive-definiteness
#' check.  By default, the tolerance is used as 1e-06
#' @return A positive definite symmetric matrix
#' @author Romain Francois
#' @seealso \code{\link{checkSymmetricPDMatrix}}
#' @keywords datagen
#' @examples
#'
#'   parseCovMatrix(2, nCov = 3)
#'   parseCovMatrix(c(1,2,3), nCov = 3)
#'   parseCovMatrix(c(1,2,4), nCov = 2)
#'
parseCovMatrix <- function(
   values,        #@ values used
   nCov,          #@ number of covariates
   tol = 1e-06
){
 	##############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# parseCovMatrix.R Fri Fri Jun 01 11:16:03 BST 2007 @469 /Internet Time/
	#
	# Author: Romain Francois
	##############################################################################
	# DESCRIPTION: parses a covarariance matrix
  # KEYWORDS: check, component:support
	##############################################################################


   ### 1st step build the matrix
   if( is.matrix(values)){
     mat <- values
   } else {
     values <- parseCharInput( values, sort = FALSE )
     if(nCov == 1){
       length(values) == 1 || ectdStop("Dimension problem")
       mat <- matrix( values[1], nrow = 1, ncol = 1) 
     } else {
       if(length(values) == 1) values <- rep(values, nCov)
       nValues <- length(values) 
       if( nValues == nCov){
         mat <- diag( values) 
       } else if( nValues == (nCov)*(nCov+1)/2 ){
         mat <- matrix( 0, ncol = nCov, nrow = nCov)
         mat[ upper.tri(mat, diag = TRUE) ] <- values
         mat <- t(mat)
         mat[ upper.tri(mat, diag = TRUE) ] <- values
       } else{
         ectdStop("Dimension Problem") 
       }
     }
   } 
   
   # 2nd step, ensure the matrix is positive definite 
   checkSymmetricPDMatrix( mat, tol )
   
   mat    
       
}
               
