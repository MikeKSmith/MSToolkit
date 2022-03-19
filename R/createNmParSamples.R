#' Simulate parameter sets from a NONMEM run object
#'
#' Creates a list containing a set of "THETA", "OMEGA" and "SIGMA" parameter
#' values, based on either the initial or final estimates of a NONMEM run
#'
#' Firstly, the function extracts the information needed to simulate. * If
#' method is "Covariance", it extracts the final estimates and covariance
#' matrix from the run * If method is "Final", it extracts the final estimates
#' from the run * If method is "Initial", it extracts the initial estimates
#' from the run
#'
#' If method is either "Final" or "Initial", then the parameters selected are
#' just repeated "N" times and returned as a single list structure (ie. no
#' difference between parameter sets)
#'
#' If method is "Covariance" and the covariance matrix can be extracted,
#' samples are taken from a multivariate normal distribution (using the
#' \link{mvrnorm} function in the \emph{MASS} library).  These samples are
#' formatted in a list and returned
#'
#' @param N Number of samples to take
#' @param run Either a NONMEM run, NONMEM control file, or NONMEM output file,
#' as imported using \emph{RNMImport}
#' @param seed Random number seed
#' @param method Parameter method: Covariance, Final or Initial
#' @return A list of length "N", where each element of the list contains: * An
#' element called "THETA", containing a vector of fixed effects * An element
#' called "OMEGA", containing a matrix of between subject effects * An element
#' called "SIGMA", containing a matrix of within subject effects
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @keywords nonmem
#'
#' @export
"createNmParSamples" <- function(
		N, 													#@ Number of samples to create
		run,												#@ NONMEM run
		seed = .deriveFromMasterSeed( ),					#@ Random seed
		method = c("Covariance", "Final", "Initial")		#@ Method for creating samples
)
{
	# Stop if library not there
	if (!requireNamespace("RNMImport")) ectdStop("RNMImport library not found")

	# Set the seed
	set.seed(seed)

	## Check inputs
	if (!is.numeric(N) || length(N) != 1 || N < 1) ectdStop("Number of samples must be a single positive integer")
	method <- match.arg(method)

	## Get parameter estimates
	switch(class(run),
		"NMRun" = {
			thVals <- switch(method, "Initial" = RNMImport::getThetas(run, "initial")[2,], RNMImport::getThetas(run))
			omVals <- switch(method, "Initial" =  RNMImport::getOmegas(run, "initial"),  RNMImport::getOmegas(run))
			sgVals <- switch(method, "Initial" = RNMImport::getSigmas(run, "initial"), RNMImport::getSigmas(run))
			theCov <- RNMImport::getEstimateCov(run)
		},
		"nmRunReport" = {
			if (method == "Initial") ectdStop("For 'initial' method, a control file object or full run must be provided")
			thisProb <- run$problemResults[[1]]
			thVals <- thisProb$FinalEstimates$THETA
			omVals <- thisProb$FinalEstimates$OMEGA
			sgVals <- thisProb$FinalEstimates$SIGMA
			theCov <- thisProb$CovarianceMatrix
		},
		"nmModel" = {
			if (method %in% c("Final", "Covariance")) ectdStop(paste("For '", method, "' method, a report file object or full run must be provided", sep=""))
			thisProb <- run$problem[[1]]
			thVals <- thisProb$Theta[,2]
			names(thVals) <- row.names(thisProb$Theta)
			omVals <- thisProb$Omega$initialMatrix
			sgVals <- thisProb$Sigma$initialMatrix
			theCov <- NULL
		},
		ectdStop("'run' input must be a NONMEM run object, a NONMEM contol file, or a NONMEM output file, as created by the RNMImport library")
	)

	## Check matrix inputs are symmetricPD
	if (length(omVals)) checkSymmetricPDMatrix(omVals)
	if (length(sgVals)) checkSymmetricPDMatrix(sgVals)

	## Return "Initial" or "Final" samples
	if (method %in% c("Initial", "Final")) {
		basePars <- list(THETA = thVals, OMEGA = omVals, SIGMA = sgVals)
		if (!length(unlist(basePars))) ectdStop(paste("Could not extract", method, "parameters"))
		return(lapply(1:N, function(i, lst) lst, lst = basePars))   # TODO: Better way of doing this?
	}

	## Get lower triangles of OMEGA and SIGMA
	if (length(omVals)) {
		omPos <- lower.tri(omVals, diag = TRUE)
		omVals <- omVals[ omPos ]
	} else omVals <- c()
	if (length(sgVals)) {
		sgPos <- lower.tri(sgVals, diag = TRUE)
		sgVals <- sgVals[ sgPos ]
	} else sgVals <- c()

	## Combine parameters into a single vector
	allPars <- c(thVals, omVals, sgVals)
	if (length(allPars) != nrow(theCov)) ectdStop(paste("Number of parameters (", length(allPars), ") does not match dimensions of covariance matrix (", nrow(theCov), ")", sep=""))

	# Get the elements from the covariance matrix
	isZero <- apply(theCov == 0, 2, all)
	subPars <- allPars[!isZero]
	subCov <- theCov[!isZero,!isZero]
	checkSymmetricPDMatrix(theCov)

	# Simulate Values
	newPars <- matrix(allPars, nrow = N, ncol = length(allPars), byrow = TRUE) 		# Build initial matrix structure
	newPars[,!isZero] <- mvrnorm(N, subPars, subCov)								# Use "mvrnorm" to create samples
	appFun <- function(vec, nTh, thN, nOm, omN, nSg, sgN) {
		newTh <- vec[1:nTh]; names(newTh) <- thN
		newOm <- createNmMatrix(vec[nTh + (1:nOm)], dimnames = omN, byrow = FALSE)
		newSg <- createNmMatrix(vec[nTh + nOm + (1:nSg)], dimnames = sgN, byrow = FALSE)
		list(THETA = newTh, OMEGA = newOm, SIGMA = newSg)
	}
	apply(newPars, 1, appFun, nTh = length(thVals), thN = names(thVals), nOm = length(omVals), omN = dimnames(omVals), nSg = length(sgVals), sgN = dimnames(sgVals))
}
