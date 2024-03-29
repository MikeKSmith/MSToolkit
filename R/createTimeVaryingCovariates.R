#' Create a set of time-varying covariates
#'
#' Creates a set of time-varying covariates from a multivariate normal
#' distribution and (optionally) a set of constraints.
#'
#'
#' @param subjects (Required) Subjects for which to create covariates
#' @param names (Required) Names for the continuous covariates.  They should be
#' valid R names (See \code{\link{validNames}}) and no duplicate name should be
#' given
#' @param mean (Required) List of means. Must be of same length than
#' \code{names}. If it is a vector, all the time point will use the same value
#' @param covariance (Optional) Lower triangle of covariance matrix. See
#' \code{\link{parseCovMatrix}} for details.  Should be a list. If it is a
#' vector, all the time point will use the same value
#' @param range (Optional) Ranges of acceptable values for each covariates. See
#' \code{\link{parseRangeCode}} for details.  It should be a list. If some
#' covariates don't have the range, the components should be set to NULL
#' @param digits (Optional) Number of digits used to round the values.  This
#' argument can be either missing (the default), so no rounding is done, of
#' length one and all variables are rounded at the same digits, of same length
#' than the number of covariates so that each covariate is rounded according to
#' its value. This argument is first parsed by \code{\link{parseCharInput}} so
#' it can either be a character vector or a numeric vector.  See
#' \code{\link{parseCharInput}}.  If the parsed \code{digits} vector does not
#' have length one or length equal to the number of covariates, an error is
#' generated by the \code{\link{ectdStop}} function.  This is missing by
#' default, resulting in no rounding being performed
#' @param maxDraws (Optional) Maximum number of attempts allowed if initial
#' data not in range (100 by default)
#' @param seed (Optional) Random seed to use.  By default, this is derived from
#' the current random seed
#' @param idCol (Optional) Name of the subject column. Must be a valid R name
#' (See \code{\link{validNames}}) and not equal to one entry of \code{names}.
#' "SUBJ" by default
#' @param timeCol (Optional) Name of the time column. Must be a valid R name
#' (See \code{\link{validNames}}) and not equal to one entry of \code{names}.
#' "TIME" by default
#' @param treatPeriod Vector of time points
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso \code{\link{createDiscreteCovariates}} to create covariates for a
#' discrete distribution.
#'
#' \code{\link{createExternalCovariates}} to create covariates by
#' \emph{sampling} data from an external file.
#'
#' \code{\link{createCovariates}} that wraps \code{createContinuousCovariates}
#' and the two other described above.
#' @keywords datagen
#' @examples
#'  # same
#'  dat <- createTimeVaryingCovariates(10, "X, Y, Z",
#' 	mean <- list(X = 1:4, Y = rep(3, 4), Z = "2.5, 3, 3.2, 3.6"),
#' 	covariance = list(1, 2:5, cbind(c(1,.5,.3,0), c(.5,1,0,0), c(.3,0,1,0), c(0,0,0,1))),
#' 	range = list("10>=X>0", NULL, c("Z>0", "Z<=10")),
#' 	idCol = "SUBJ", timeCol = "TIME", treatPeriod = c(0.25, 0.5, 1, 12))
#'
#' @export
createTimeVaryingCovariates <- function(
		subjects,
		names,
		mean,
		covariance = 0,
		range = NULL,
		digits,
		maxDraws = 100,
		seed = .deriveFromMasterSeed(),
		idCol = getEctdColName("Subject"),
		timeCol = getEctdColName("Time"),
		treatPeriod
){
	set.seed( seed )

	## sanity checks on the inputs
	if( missing(mean) ) ectdStop("`mean` is needed in `createTimeVaryingCovariates`")
	subjects <- .expandSubjects( subjects )
	nSubjects <- get("nSubjects")

	# mean list
	if (is.list(mean)) {
		mean <- lapply(mean, parseCharInput)
		if (!all(sapply(mean, length) == length(mean[[1]]))) ectdStop("the time length of all the covariates should be the same")
	} else {
		mean  <- list(parseCharInput( mean  ))
	}

	# number of covariates and time points
	nCov <- length( mean )
	nTime <- length(mean[[1]])

	# check the length of 'treatPeriod' argument
	if (missing(treatPeriod)) ectdStop("`treatPeriod` is required when creating time-varying covariates")
	if (length(treatPeriod) != nTime) ectdStop("the length of `treatPeriod` must be equal to the number of time points")

	# check names
	names <- if(missing(names)) {
		"X" %.% 1:nCov
	} else {
		parseCharInput( names , checkdup = TRUE, convertToNumeric = FALSE)
	}

	validNames( names, idCol )

	if(length(names) != length(mean)) {
		ectdStop(
				"Dimension mismatch between `names` and `mean`"  %.nt%
				"`mean`  of length: " %.% length(mean) %.nt%
        		"`names` of length: "%.% length(names) )
	}

	# covariance list
	if (is.list(covariance)) {
		covariance <- lapply(covariance, parseCovMatrix, nTime)
		if (length(covariance) != nCov) ectdStop("the length of covariance list should be equal to the number of covariates")
	} else {
		covariance <- parseCovMatrix( covariance, nTime )
		covariance <- rep(list(covariance), nCov )
		if (nCov > 1) warning("there is only 1 covariance matrix, use it for all the time point")
	}


	# maxDraws
	maxDraws <- as.integer(maxDraws)
	if( maxDraws < 1 ) ectdStop("The maximum number of draws should be a positive integer")

	# digits
	if( !missing(digits) && digits < 0) ectdStop("The `digits` argument must be positive")

	# range
	if( is.null(range) ) {
		range <- rep(list(NULL), nCov)
	} else if (is.list(range)) {
		range <- lapply(range, parseRangeCode)
		if (length(range) != nCov) ectdStop("the length of range list should be equal to the number of covariates")
	} else {
		ectdStop("The `range` argument must be list for every covariance")
	}

	OUT <- NULL
	for (i in 1:nCov) {
		rangei <- range[[i]]
		namei <- names[i]
		namev <- paste("T", treatPeriod, sep = ".")
		if (!is.null(rangei)) rangei <- sapply(1:nTime, FUN = function(X) gsub(namei, paste("T", treatPeriod[X], sep = "."), rangei, fixed = TRUE))

		tmp <- createContinuousCovariates(
				subjects = subjects,
				names = namev,
				mean = mean[[i]],
				covariance = covariance[[i]],
				range = rangei,
				digits = digits,
				maxDraws = maxDraws,
				seed = seed,
				idCol = idCol,
				includeIDCol = TRUE)

		tmp <- stats::reshape(tmp, idvar = idCol, varying = list(2:ncol(tmp)),
				timevar = timeCol, v.names = namei, times = treatPeriod, direction = "long")
		rownames(tmp) <- NULL

		if (is.null(OUT)) {
			OUT <- tmp
		} else {
			OUT <- merge(OUT, tmp)
		}
	}

	OUT <- OUT[ do.call("order", OUT[c(idCol, timeCol)]), , drop=FALSE]
	return(OUT)

}

