#' Checks if a matrix is symmetric and positive definite
#'
#' Checks if a matrix is symmetric and positive definite using an eigen value
#' decomposition.
#'
#' @param mat (Required) The matrix to check
#' @param tol (Optional) The tolorence to use when comparing the value to 0.
#' The default is "1e-06"
#' @return Returns an error if the matrix does not satisfy symmetric or
#' positive definite. Otherwise does not return anything
#' @author Romain Francois
#' @keywords error
#' @examples
#'   checkSymmetricPDMatrix( diag(4))
#'   A <- matrix( c( 2, -1, 0, -1, 2, -1, 0, -1, 2 ), nrow=3, byrow=TRUE )
#'   checkSymmetricPDMatrix( A )
#'   \dontrun{
#'   # Not symmetric
#'   B <- matrix( c( 1, 2, 3, 4, 5, 6, 7, 8, 9), nrow=3, byrow=TRUE )
#'   checkSymmetricPDMatrix(B)
#'   # Not positive definite
#'   C <- matrix( c( -2, 1, 0, 1, -2, 1, 0, 1, -2 ), nrow=3, byrow=TRUE )
#'   checkSymmetricPDMatrix(C)
#'   }
#'
#' @export
checkSymmetricPDMatrix <- function( mat, tol = 1e-6 ){
 	##############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# checkSymmetricPDMatrix.R Fri Jun 01 13:13:38 BST 2007 @551 /Internet Time/
	#
	# Author: Romain Francois
	##############################################################################
	# DESCRIPTION: checks if a matrix is symmetric and positive definite
  # KEYWORDS: component:support
	##############################################################################

  if(any(is.na(mat)))
    ectdStop("Missing values not allowed in covariance matrix")

  if( diff(dim(mat)) != 0)
    ectdStop("Input matrix not square")

  if( !isSymmetric(mat) )
    ectdStop( "Input matrix not symmetric" )

  ev <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values

  # This test for positive-definiteness was found in the function mvrnorm
  if (!all(ev >= -tol * abs(ev[1])))
      ectdStop("matrix not positive definite")

}

