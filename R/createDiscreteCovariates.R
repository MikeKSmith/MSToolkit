#' Create covariates from a discrete distribution
#'
#' The \code{values} and \code{probs} argument are parsed using the
#' \code{\link{parseHashString}} helper function. They could be either :
#'   - a vector giving the values for each variable.
#'     \code{c("1,2", "1,2,3")} would mean that the first variable takes values
#'     1 and 2, and the second variable takes values 1, 2 and 3.
#'   - a list giving the values for each variable.
#'   \code{list(c(1,2), c(1,2,3))} would mean that the first variable takes
#'   values 1 and 2, and the second variable takes values 1, 2 and 3.
#'   - a compact notation using the hash symbol to separate variables
#'   \code{"1,2#1,2,3"}
#'
#' Additionally for the \code{probs} argument, a check is performed to make
#' sure that each variable probability sums to 1.
#'
#' Alternatively, a \code{probArray} argument can be given. This should be a
#' data frame containing one more column (named "prob") than the number of
#' variables to create. Each variable has a column which contains the values it
#' can take. The prob column gives the probability for each combination. See
#' examples. The prob column should sum up to one.
#'
#' @param subjects (Required) Vector of subjects (or number of subjects) for
#' which to create covariates
#' @param names (Required) Names of the discrete covariates to be created.  All
#' the names should be valid R names. See \code{link{validNames}}.
#' @param values (Required) Values that the covariates can take. See details
#' section.
#' @param probs (Optional) Probabilities for each covariates. See details
#' section.
#' @param probArray (Optional) Probability array for uneven sampling. See
#' details section.
#' @param seed (Optional) Random seed to use.By default, it is based on the
#' current random seed
#' @param idCol (Optional) Name of the subject column.  Must be a valid R name
#' (see \code{\link{validNames}}) and not be duplicated with any \code{names}.
#' "SUBJ" by default
#' @param includeIDCol (Optional) A logical value. Should the subject column be
# included.  Typically, the \code{\link{createCovariates}} function would set
# this to FALSE as it does not need it.  TRUE by default
#' @return A data frame.
#' @seealso \code{\link{createContinuousCovariates}},
# \code{\link{createExternalCovariates}},
# \code{\link{createTimeVaryingCovariates}}, and
# \code{\link{createCovariates}}
#' @keywords datagen
#' @examples
#'
#'
#'   # 10 samples of X and Y where:
#'   # P[ X = 1 ] = .1
#'   # P[ X = 2 ] = .9
#'   # -
#'   # P[ Y = 7 ] = .5
#'   # P[ Y = 8 ] = .4
#'   # P[ Y = 9 ] = .1
#'   createDiscreteCovariates( 10 ,
#'                             names = "X, Y",
#'                             probs = ".1,.9#.5,.4,.1",
#'                             values = "1,2#7,8,9")

#'   # using the probArray version
#'   pa <- data.frame( F1 = rep(0:1, 3),
#'                     F2 = rep(1:3, each = 2),
#'                     PROB = c(.1,.2,.1,.2,.2,.2) )
#'
#'   createDiscreteCovariates( 100 , probArray = pa )
#'
createDiscreteCovariates <- function(
  subjects,     #@ Subjects for which to create covariates
  names,        #@ Names for the continuous covariates
  values,       #@ Values from which to sample for each covariate
  probs,        #@ Probabilities for each variable
  probArray,    #@ An array of probabilities for multivariate sampling
  seed  = .deriveFromMasterSeed( ), #@ Random number generation seed
  idCol = getEctdColName("Subject"),
  includeIDCol  = TRUE
)
{
 	##############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# createDiscreteCovariates.R Fri Jun 01 10:42:47 BST 2007 @446 /Internet Time/
	#
	# Author:
	##############################################################################
	# DESCRIPTION: create a set of discrete covariates
    # KEYWORDS: datagen, component:covariate
	##############################################################################

	set.seed(seed)
	names  <- if( missing(names) && !missing(probArray) ) {
				if (is.array(probArray)) ectdStop(
				  "If probArray is supplied as an array, the names input must be
				  provided")
				colnames( probArray )[-ncol(probArray)]
			} else {
				parseCharInput( names  ,
				                convertToNumeric = FALSE,
				                checkdup = TRUE)
			}
	validNames( names, idCol )

	## handle `probs` or `probArray`
	if( missing(probs) ){
		if(missing(probArray)){
			ectdStop("need one of `probArray` or `probs`")
		}
		# Attempt to deduce values input from the probArray argument
		if( missing(values)){
			if( is.array( probArray) ){
				values <- dimnames(probArray)
				if (!length(values)) ectdStop(
				  "If probArray input is supplied as an array, either the array must
				  have dimension names or the 'values' input must be provided")

				values <- lapply(values, function(x) {
							numTry <- suppressWarnings(as.numeric(x))
							if (any(is.na(numTry))) return(x) else return(numTry)
						}
				)
			} else {
				values <- lapply( probArray[,-ncol(probArray),drop = FALSE], unique )
			}
		} else {
			values <- parseHashString( values )
		}
		if( !.allSameLength(values, names) ) ectdStop(
		  "`names` and `values` inputs must have the same length")
	} else {
		values <- parseHashString( values, convertToNumeric = FALSE)
		probs  <- parseHashString( probs, checkProb = TRUE  )
		if( !.allSameLength(values, probs, names) ) ectdStop(
		  "`names`, `probs` and `values` must have the same length")
		if( any(sapply(probs,length) != sapply(values, length)) ) ectdStop(
		  "items in `values` and `probs` must have the same length")
		names(probs)  <- names
	}

	names(values) <- names

	subjects <- .expandSubjects( subjects )
	nSubjects <- get("nSubjects")

	grid <- .handleProbArray( probArray, values, probs)

	indexes <- sample( 1:nrow(grid),
	                   prob = grid[,ncol(grid)],
	                   size = nSubjects ,
	                   replace = TRUE)

	out <- grid[indexes, names, drop = FALSE]

	if( includeIDCol ) out <- .eval( "data.frame( $idCol = subjects, out)" )

	rownames(out) <- 1:nSubjects
	out
}
