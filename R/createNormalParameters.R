#' Create parameters based on a multivariate normal distribution
#'
#' Creates parameter variables by sampling from a multivariate normal
#' distribution. Parameter values can vary between simulation replicates, but
#' also between subjects within each simulation replicate (between subject
#' variability).
#'
#' The function first samples a new value for the parameter. This parameter
#' value is used for each subject within the trial replicate. These values are
#' generated from a multivariate normal distribution with mean and covariance
#' matrix specified within the function arguments. These can be viewed as the
#' "fixed effects" for the trial replicate.
#'
#' If "betNames" argument has been supplied ("betNames" must be a subset of the
#' parameter names supplied in the "names" argument) then another sample will be
#' taken to generate subject-specific parameter values. If the "betMean" input
#' has not been supplied, the mean of the between effects distribution will be
#' 0.  Samples for each subject will be taken from a multivariate normal
#' distribution with mean "betMean" and covariate matrix "betCov". "betCov"
#' specifies between subject variability in the parameters.
#'
#' If fixed and between subject effects have been generated, the "errStruc"
#' input will specify how the between subject effects are to be applied:
#'
#'   * None = Keep between subject effect separate (and use the "suffix"
#'   supplied to name the variables)
#'   * Additive = Add each between subject effect to the corresponding fixed
#'   effect
#'   * Proportional = Multiply the fixed effect by the exponentiated between
#'   subject effect
#'   * Log-Normal = Add "1 + each between subject effect" to the corresponding
#'   fixed effect
#'
#' If the "range" argument is provided, the output from the above algorithm
#' (fixed + between) is checked against the acceptance range.  If the data is
#' not within range, the function will take "maxDraws" more attempts at the
#' above algorithm to try and generate "in range" data.  If that is not
#' possible, the function will stop and return an error.
#'
#' @param subjects (Required) Subjects for which to create parameters
#' @param names (Required) Names of parameters to generate
#' @param mean (Required) Means for fixed parameters
#' @param covariance (Optional) Covariance matrix for fixed parameters.  By
#' default, this is a matrix of zeros, representing no error to be included
#' @param range (Optional) Range of acceptable values.  Missing by default,
#' resulting in no "range" of values is applied to the data
#' @param betNames (Optional) Between subject effects to create.  Default is to
#' not create between subject data
#' @param betMean (Optional) Means for the between subject effects.  Default is
#' to not create between subject data
#' @param betCov (Optional) Covariance matrix for the between subject effects.
#' Default is to not create between subject data
#' @param errStruc (Optional) (None) Function to map between subject effects:
#' Additive, Proportional, Log-Normal or None
#' @param suffix (Optional) Suffix to use for retain between subject effects
#' (default is ".Between")
#' @param idCol (Optional) Subject variable name for return data ("SUBJ" by
#' default)
#' @param maxDraws (Optional) Maximum number of iterations for valid parameters
#' (10 by default)
#' @param seed (Optional) Random number generation seed.  By default, this is
#' dervied from the current random seed
#' @param flagName (Optional) Flag name for parameters out of bounds ("PAROMIT"
#' by default)
#' @param digits (Optional) Number of digits to which to round generated
#' continuous parameters.  Can be an atomic integer, in which case all
#' variables are rounded according to it. Can be a numeric vector of the same
#' length of the number of fixed parameters, in which case each parameter is
#' rounded according to its value.  The default is 3
#' @param parRangeTolerance (Optional, default .9) Proportion of subjects with
#' parameters in specified range that is acceptable for us to continue
#' @return A data frame containing parameter data
#' @note If earlier versions of MSToolkit, the "Proportional" error structure
#' was implemented as exp(fixed + between subject)
#' @author Rich Pugh
# @seealso \code{\link{createParameters}} and
# \code{\link{createExternalParameters}}
#' @keywords datagen
#' @examples
#'
#' createNormalParameters(5, "E0,ED50,EMAX",
#'                        mean = c(0, 50, 100),
#'                        covariance = diag(c(1, 5, 10)))
#' #  SUBJ       E0     ED50     EMAX   PAROMIT
#' #1    1 -0.8356286 50.41064 98.01898       0
#' #2    2 -0.8356286 50.41064 98.01898       0
#' #3    3 -0.8356286 50.41064 98.01898       0
#' #4    4 -0.8356286 50.41064 98.01898       0
#' #5    5 -0.8356286 50.41064 98.01898       0
#'
#' createNormalParameters(5, "E0,ED50,EMAX",
#'                        mean = c(0, 50, 100),
#'                        covariance = diag(c(1, 5, 10)),
#'                        betNames = c("E0", "EMAX"),
#'                        betCov = diag(2),
#'                        errStruc = "Additive")
#' #  SUBJ       E0     ED50     EMAX   PAROMIT
#' #1    1  0.7596522 50.41064 98.59476       0
#' #2    2 -0.5061208 50.41064 97.71359       0
#' #3    3 -1.6560970 50.41064 99.53076       0
#' #4    4 -0.3481996 50.41064 98.40882       0
#' #5    5 -0.0973039 50.41064 97.39774       0
#'
#' # no covariance by default
#' createNormalParameters(5, "E0,ED50,EMAX", mean = c(0, 50, 100) )
#' #  SUBJ E0 ED50 EMAX PAROMIT
#' #1    1  0   50  100       0
#' #2    2  0   50  100       0
#' #3    3  0   50  100       0
#' #4    4  0   50  100       0
#' #5    5  0   50  100       0
#'
"createNormalParameters" <- function(
	subjects,
	names,
	mean,
	covariance = 0,
	range,
	betNames,
	betMean = 0,
	betCov = 0,
	errStruc = c("None", "Proportional", "Additive", "Log-Normal"),
	suffix = ".Between",
	idCol = getEctdColName("Subject"),
	maxDraws = 10,
	seed = .deriveFromMasterSeed( ),
	flagName = getEctdColName("ParOmit"),
	digits = 3,
	parRangeTolerance = .5
) {
	##############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# createNormalParameters.R Wed Jun 20 11:39:52 BST 2007 @486 /Internet Time/
	#
	# Author: Rich P
	##############################################################################
	# DESCRIPTION: create parameters from a normal distribution
	# KEYWORDS: datagen, component:data:allocate
	##############################################################################

  	# Set the seed
	set.seed(seed)

	## initial tests
	subjects <- .expandSubjects( subjects )
	nSubjects <- length(subjects)
	mean <- parseCharInput( mean )
	nFixed <- length(mean)
	idCol <- parseCharInput( idCol,
	                         expected = 1,
	                         convertToNumeric = FALSE,
	                         valid = TRUE)
	flagName <- parseCharInput( flagName,
	                            expected = 1,
	                            convertToNumeric = FALSE,
	                            valid = TRUE)
	maxDraws <- parseCharInput( maxDraws,
	                            expected = 1,
	                            convertToNumeric = TRUE)
	digits <- parseCharInput( digits,
	                          convertToNumeric = TRUE)

	names <- if(missing(names)) {
    	"X" %.% 1:nFixed
	} else {
    	parseCharInput( names,
    	                convertToNumeric = FALSE,
    	                checkdup = TRUE,
    	                expected = nFixed,
    	                valid = TRUE )
	}
	covariance <- parseCovMatrix( covariance, nFixed )

	# Checks on digits, maxDraws and errStruc
	getErrStruc <- try(match.arg(errStruc))
	if (class(getErrStruc) == "try-error") {
		errStruc <- casefold(substring(errStruc, 1, 1),
		                     upper = TRUE)
		getErrStruc <- try(match.arg(errStruc))
		if (class(getErrStruc) == "try-error") ectdStop(
		  "Error Structure input should be 'None', 'Additive', 'Log-Normal' or
		  'Proportional'")
	}
	if (maxDraws < 1) ectdStop(
	  "The maximum number of draws should be a positive integer")
	if (any(digits < 0)) ectdStop(
	  "The `digits` argument must be positive")

	## Generate parameters depending on presence of "range" input
	if (missing(range)) {

		# Generate 1 set of fixed effects
		fixed <- .generateFixedPars(mean, covariance, nSubjects, names)

		## Now deal with the between subject effects
		if ( !missing(betNames) ) {

			# Generate a set of between subject parameters
			betNames <- parseCharInput( betNames,
			                            convertToNumeric = FALSE,
			                            checkdup = TRUE,
			                            valid = TRUE )
			between <- .generateBetweenPars(betNames,
			                                names,
			                                betMean,
			                                betCov,
			                                nSubjects,
			                                suffix)
			omitFlag <- rep(0, nSubjects)
			out <- .combineFixedAndBetween(fixed,
			                               between,
			                               betNames,
			                               getErrStruc,
			                               suffix,
			                               digits)

		}
		else {   # no between
			out <- .roundIt( fixed, digits )
		}

		# Combine data
		out <- data.frame( subjects, out, rep(0, nSubjects))
		names(out)[c(1, length(out))] <- c(idCol, flagName)

	}
	else {
		# Loop over draws allowed
		for (i in 1:maxDraws) {
			fixed <- .generateFixedPars(mean, covariance, nSubjects, names)

			## Now deal with the between subject effects
			if ( !missing(betNames) ) {

				# Generate a set of between subject parameters
				betNames <- parseCharInput( betNames,
				                            convertToNumeric = FALSE,
				                            checkdup = TRUE,
				                            valid = TRUE )
				between <- .generateBetweenPars(betNames,
				                                names,
				                                betMean,
				                                betCov,
				                                nSubjects,
				                                suffix)
				omitFlag <- rep(0, nSubjects)
				out <- .combineFixedAndBetween(fixed,
				                               between,
				                               betNames,
				                               getErrStruc,
				                               suffix,
				                               digits)

			}
			else {   # no between
				out <- .roundIt( fixed, digits )
			}

			# Combine data
			out <- data.frame( subjects, out, rep(0, nSubjects))
			names(out)[c(1, length(out))] <- c(idCol, flagName)

			# Check to see if the data is in range
			out <- .checkParameterRange(out, range, flagName)
			if (all(out[[flagName]] == 0)) break

			# Stop if we have exhausted our number of draws
			if (i == maxDraws) {
				subjectFlags <- out[[flagName]][!duplicated(out[[idCol]])]
				propOk <- 1 - sum(subjectFlags) / length(subjectFlags)
				roundPercent <- round(100 * propOk, 1)
				basicMessage <- paste("After ",
				                      maxDraws,
				                      " attempts, ",
				                      roundPercent,
				                      "% of subjects have parameters in acceptable
				                      range",
				                      sep="")
				if (propOk >= parRangeTolerance) ectdWarning( basicMessage )
				else ectdStop( basicMessage )
			}
		}
	}

	rownames(out) <- 1:nSubjects
	out
}

".generateFixedPars" <- function(mean, cov, N, colNames) {
	fixed <- as.data.frame( matrix( MASS::mvrnorm( n = 1,
	                                         mu = mean,
	                                         Sigma = cov ),
	                                nrow = 1 ) )
	names(fixed) <- colNames
	fixed[rep(1, N), , drop = FALSE]
}

".generateBetweenPars" <- function(betNames, fixedNames, mean, cov, N, suffix) {

	myTest <- betNames %in% fixedNames
	if ( !all(myTest)) {
		outMessage <- paste(
		"Some between subject effects don't have a corresponding fixed parameter:",
		  paste(betNames[!myTest],
		        collapse=", "))
		ectdStop( outMessage )
	}

	# Get the covariance matrix + means
	nBetween <- length( betNames )
	betCov   <- parseCovMatrix(cov, nBetween )
	betMean <- rep(parseCharInput(mean), length.out = nBetween)

	# Generate the data once
	between <- as.data.frame(  MASS::mvrnorm( N, mu = betMean, Sigma = betCov ) )

	## apply the suffix to the between data and return it
	names( between ) <- paste(betNames, suffix, sep="")
	between
}

".combineFixedAndBetween" <- function(fixed, between, betNames, errStruc, suffix, digits) {

	switch( errStruc,
		"Additive" = {
			out <- fixed
			for ( bn in betNames ) out[[ bn ]] <- out[[ bn ]] +
			    between[[ paste(bn, suffix, sep="") ]]
			out <- .roundIt( out, digits )
		},
		"Log-Normal" = {
			out <- fixed
			for( bn in betNames ) out[[ bn ]] <- out[[ bn ]] *
			    exp( between[[ paste(bn, suffix, sep="") ]] )
			out <- .roundIt( out, digits )
		},
		"Proportional" = {
			out <- fixed
			for( bn in betNames ) out[[ bn ]] <- out[[ bn ]] *
			    ( 1 + between[[ paste(bn, suffix, sep="") ]] )
			out <- .roundIt( out, digits )
		},
		"None" = {
			digits <- parseCharInput( digits, convertToNumeric = TRUE)
			fixed   <- .roundIt( fixed, digits )
			if ( length(digits) == 1 ||
			     ncol(fixed) == ncol(between) ) between <- .roundIt( between, digits)
			else between <- .roundIt( between,
			                          digits = digits[ names(fixed) %in% betNames ] )
			out <- data.frame( fixed, between)
		}
	)
	out
}

".checkParameterRange" <- function(out, range, flagName) {

	range <- parseRangeCode( range )		# Parse the range code
	alright <- try( eval( range, out ) )
	if (class(alright) == "try-error" ||
	    length(alright) != nrow(out)) out[[flagName]] <- rep(1, nrow(out))
	else out[[flagName]] <- as.numeric(!alright)
	out

}
