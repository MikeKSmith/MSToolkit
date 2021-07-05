#' Creates a matrix based on a "NONMEM style" vector
#' 
#' Creates a matrix based on a vector specifying the lower diagonal elements
#' (as would be defined in NONMEM outputs)
#' 
#' Places the values in the lower diagonal of an output matrix, and reflects
#' these values across the diagonal.  Adds dimension names if provided
#' 
#' @param x Vector of values to be placed in the matrix
#' @param dimnames Optional dimension names for output matrix
#' @param byrow (Optional) Should we read in the data into the matrix diagonal
#' by row (default) or by column
#' @return A matrix
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @keywords matrix nonmem
#' @examples
#' 
#' 	createNmMatrix(1:3)
#' 	createNmMatrix(1:6)
#' 	createNmMatrix(1:10, list(LETTERS[1:4], letters[1:4]))
#' 
"createNmMatrix" <- function(
		x, 						#@ Input vector
		dimnames = NULL,		#@ Optional dimension names
		byrow = TRUE			#@ Add values by row (NONMEM default)
) {
	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# createNmMatrix.R 11NOV09
	# 
	# Author: Rich
	###############################################################################
	# DESCRIPTION: Build symmetric matrix based on vector of "lower" triangle
	###############################################################################
	if(!length(x)) return(NULL)
	if(is.matrix(x)) {
		if(nrow(x) != ncol(x)) ectdStop("Cannot convert matrix to symmetric matrix - differing number of rows and columns")
		outMat <- replace(t(x), lower.tri(x, diag = TRUE), x[lower.tri(x, diag = TRUE)])
	}
	else {
		N <- round((sqrt(8 * length(x) + 1) - 1)/2) 					# Number of rows/columns of the matrix based on length of vector
		myMat <- array(0, c(N, N), dimnames = dimnames)										# Add values to the upper triangle
		if (length(x) != sum(lower.tri(myMat, diag = T))) ectdStop("Incorrect number of inputs with which to create matrix")
		if (byrow) {
			myMat <- replace(myMat, !lower.tri(myMat), x)							# Add values to the upper triangle
			outMat <- replace(myMat, lower.tri(myMat, T), t(myMat)[lower.tri(myMat, TRUE)])		# Make symmetric
		}
		else {
			myMat <- replace(myMat, lower.tri(myMat, diag = TRUE), x)
			outMat <- replace(t(myMat), lower.tri(myMat, diag = TRUE), x)
		}
	}
	outMat
}
