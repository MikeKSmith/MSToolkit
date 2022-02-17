#' Generate simulated data replicates
#' 
#' Generate Simulated Data Replicates by controlling dosing, covariates,
#' parametes, response, missingness and interims
#' 
#' 
#' The generateData function calls the low level generate data components to
#' create sets of simulated data.  The following components are called to
#' create aspects of the simulated trial data:
#' 
#' \code{\link{createTreatments}}: Used to create a dataset of all possible
#' treatment regimes to be allocated to subjects
#' 
#' \code{\link{allocateTreatments}}: Use to allocate treatments to subjects in
#' the simulated study
#' 
#' \code{\link{createCovariates}}: Creates a set of fixed covariates for a
#' simulated population
#' 
#' \code{\link{createParameters}}: Creates simulated fixed and between subject
#' parameters for subjects in each replicate
#' 
#' \code{\link{createResponse}}: Creates a simulated response variable based on
#' available derived data
#' 
#' \code{\link{createMCAR}}: Adds a simulated "missing" flag to the data
#' 
#' \code{\link{createDropout}}: Adds a simulated "missing" flag to the data
#' based on a dropout function
#' 
#' \code{\link{createInterims}}: Assigns subjects in the study to interim
#' analyses
#' 
#' The function iteratively builds and combines the data components for each
#' replicte, and stores the data in the "ReplicateData" subdirectory of the
#' working directory.  This data can then be analyzed using a call to the
#' \code{\link{analyzeData}} function.
#' 
#' @param replicateN (Required) Number of replicates for which to create
#' simulated data
#' @param subjects (Required) Number of subjects in simulation
#' @param treatSubj (Optional) Number of subjects to which to allocate
#' treatments, or a vector of allocations
#' @param treatDoses (Optional) Vector of numeric treatment doses.  By default,
#' this is the same as the "subjects" input
#' @param treatSeq (Optional) Treatment matrix for crossover designs.  Missing
#' by default, but this is required when treatType is set to "Crossover"
#' @param treatType (Optional) Treatment type: 'Parallel' or 'Crossover'.
#' Default is "Parallel"
#' @param treatPeriod (Optional) Vector of numeric treatment time points.
#' Missing by default, resulting in no "time" element in the generated data
#' @param genParNames (Optional) Names of fixed effects to generate.  Missing
#' by default, resulting in no fixed parameters being created
#' @param genParMean (Optional) Means for generating fixed parameters.  Missing
#' by default
#' @param genParVCov (Optional) Covariance matrix for generating fixed
#' parameters.  By default, this is a matrix of zeros
#' @param respEqn (Required) Formula for creating the simulated response
#' @param respName (Optional) Response variable name.  Default is "RESP"
#' @param treatProp (Optional) Proportions for sampling.  Missing by default,
#' resulting in unbiased sampling
#' @param treatOrder (Optional) Logical flag: should allocations be assigned in
#' order.  FALSE by default
#' @param conCovNames (Optional) Continuous covariate names.  Missing by
#' default, resulting in no continuous covariates being created
#' @param conCovMean (Optional) Continuous covariate means.  Missing by default
#' @param conCovVCov (Optional) Continuous covariate covariance matrix.
#' Missing by default
#' @param conCovCrit (Optional) Continuous covariate acceptable range.  Missing
#' by default
#' @param conCovDigits (Optional) Continuous covariate rounding digits.  3 by
#' default
#' @param conCovMaxDraws (Optional) Continuous covariate maximum draws.  100 by
#' default
#' @param disCovNames (Optional) Discrete covariate names.  Missing by default,
#' resulting in no discrete covariates being created
#' @param disCovVals (Optional) Discrete covariate values.  Missing by default
#' @param disCovProb (Optional) Discrete covariate probabilities.  Missing by
#' default
#' @param disCovProbArray (Optional) Array of probabilities for multivariate
#' sampling.  Missing by default
#' @param extCovNames (Optional) Names for the continuous covariates.  Missing
#' by default, resulting in no imported covariates
#' @param extCovFile (Optional) File from which to import (including full or
#' relative path).  Missing by default
#' @param extCovSubset (Optional) Subset to apply to data.  Missing by default
#' @param extCovRefCol (Optional) Reference variable.  Missing by default
#' @param extCovSameRow (Optional) Logical flag: should covariates sampled be
#' from the same row.  TRUE by default
#' @param extCovDataId (Optional) Subject variable name from file.  Same as
#' "idCol" by default
#' @param timeCovNames (Optional) Time-varying covariate names.  Missing by
#' default, resulting in no Time-varying covariates being created
#' @param timeCovMean (Optional) Time-varying covariate means.  Missing by
#' default
#' @param timeCovVCov (Optional) Time-varying covariate covariance matrix.
#' Missing by default
#' @param timeCovCrit (Optional) Time-varying covariate acceptable range.
#' Missing by default
#' @param genParCrit (Optional) Range of acceptable values for generated fixed
#' effects.  Missing by default
#' @param genParBtwNames (Optional) Between subject effects to generate.
#' Missing by default, resulting in no created between subject effects
#' @param genParBtwMean (Optional) Means for generated between subject effects.
#' Missing by default
#' @param genParBtwVCov (Optional) Covariance matrix for generated between
#' subject effects.  Missing by default
#' @param genParErrStruc (Optional) Function to map generated effects:
#' Additive, Proportional or None.  "None" by default
#' @param genParMaxDraws (Optional) Maximum number of iterations to generate
#' valid parameters.  100 by default
#' @param genParRangeTolerance (Optional) Proportion of subjects with "in
#' range" parameter data that we're happy proceeding with
#' @param extParFile (Optional) File name for external parameter data to
#' import.  Missing by default, resulting in no imported parameter variables
#' @param extParNames (Optional) Names of parameters to import from external
#' file.  Missing by default
#' @param extParBtwNames (Optional) Between subject effects variables to import
#' from external file.  Missing by default
#' @param extParBtwNums (Optional) Integer mapping between random and fixed
#' effects in imported parameter data.  Missing by default
#' @param extParSubset (Optional) Subsets to be applied to imported parameter
#' before sampling.  Missing by default
#' @param extParCrit (Optional) Acceptance range for imported parameter columns
#' @param extParErrStruc (Optional) Function to map effects from imported
#' parameter data: Additive, Proportional or None.  "None" by default
#' @param extParRefColData (Optional) Reference column in imported parameter
#' data.  Missing by default
#' @param extParRefColName (Optional) Reference column name from imported
#' parameter data.  Missing by default
#' @param extParDataId (Optional) Subject variable name in external parameter
#' file.  Same as "idCol" by default
#' @param respInvLink (Optional) Inverse link function for the linear
#' predictor.  Missing by default, resulting in no inverse link to be applied
#' @param respDist (Optional) Outcome response variable distribution ("Normal"
#' by default)
#' @param respVCov (Optional) Residual error (co)variance to apply to generated
#' response.  None by default
#' @param respErrStruc (Optional) Function describing how to apply residual
#' error to the generated response: Additive, Log-Normal or Proportional.
#' "Additive" by default
#' @param respCrit (Optional) Range of acceptable values for created response.
#' Missing (no criteria) by default
#' @param respDigits (Optional) Number of digits to which to round the created
#' response.  3 by default
#' @param mcarProp (Optional) Proportion of observations to set to missing at
#' random.  0 by default
#' @param mcarRule (Optional) Rule to specify which observations of the data
#' should be included for MCAR allocation.  Missing by default
#' @param dropFun (Optional) User defined function to define criteria for
#' subject dropout.  Missing (no dropout) by default
#' @param dropFunExtraArgs (Optional) Additional arguments to the dropout
#' function.  None by default
#' @param interimSubj (Optional) Proportion of total subjects to be assigned to
#' each interim analysis.  Missing by default, resulting in no "interim"
#' variable derived
#' @param interimMethod (Optional) Method for creating interim variable:
#' 'Sample' or 'Proportion'.  "Sample" by default
#' @param seed (Optional) Random seed.  By default, this is derived from the
#' current session random seed
#' @param idCol (Optional) Subject variable name ("SUBJ" by default)
#' @param doseCol (Optional) Dose variable name ("DOSE" by default)
#' @param timeCol (Optional) Time variable name ("TIME" by default)
#' @param trtCol (Optional) Treatment variable name ("TRT" by default)
#' @param parOmitFlag (Optional) Parameter omit flag name ("PAROMIT" by
#' default)
#' @param respOmitFlag (Optional) Response omit flag name ("RESPOMIT" by
#' default)
#' @param missingFlag (Optional) Missingness flag name ("MISSING" by default)
#' @param interimCol (Optional) Interim variable name ("INTERIM" by default)
#' @param parBtwSuffix (Optional) Suffix for retained between subject effects
#' variables.  Suffix ".Between" is used by default
#' @param deleteCurrData (Optional) Should existing data be deleted before
#' starting generation phase (TRUE by default)
#' @param covDiff (Optional) Should covariates differ between replicates (TRUE
#' by default)
#' @param treatDiff (Optional) Should treatment allocation differ between
#' replicates (TRUE by default)
#' @param workingPath (Optional) Working directory from which to create data.
#' By default, the current working directory is used
#' @return No value is returned from the generateData function.  However, as a
#' side effect, a number of simulated replicate datasets are created.
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso \code{\link{createTreatments}}, \code{\link{allocateTreatments}},
#' \code{\link{createCovariates}}, \code{\link{createParameters}},
#' \code{\link{createResponse}}, \code{\link{createMCAR}},
#' \code{\link{createDropout}}, \code{\link{createInterims}} and
#' \code{\link{analyzeData}}
#' @keywords datagen
#' @examples
#' 
#' 
#' \dontrun{
#' generateData( replicateN = 500, subjects = 400, treatDoses = c(0, 5, 25, 50, 100), 
#'   conCovNames = c("wt", "age"), conCovMean = c(83, 55) , conCovVCov = c(14,10)^2 , 
#'   conCovDigits = 1, conCovCrit = "18 <= age <= 65", 
#'   genParNames = "E0,ED50,EMAX", genParMean = c(2,50,10), genParVCov = diag( c(.5,30,10) ), 
#'   genParBtwNames = "E0,ED50,EMAX", genParBtwMean = c(0,0,0), genParBtwVCov = diag(3), 
#'   respEqn = "E0 + ((DOSE * EMAX)/(DOSE + ED50))",  respVCov = 5, 
#'   interimSubj = ".3,.7")
#' }
#' 
generateData <- function(
		 replicateN ,	                              #@  Number of replicates
		 subjects = NULL,	                          #@ Number of subjects in simulation
		 treatSubj = subjects,	                      #@ Number of subjects to which to allocate treatments, or a vector of allocations
		 treatDoses ,	                              #@ Treatment doses
		 treatSeq ,	                                  #@ Treatment matrix for crossover designs
		 treatType = "Parallel",	                  #@ Treatment type: Parallel or Crossover
		 treatPeriod ,	                              #@ Treatment time points
		 genParNames ,	                              #@ Names of fixed effects to generate
		 genParMean ,	                              #@ Means for generating fixed parameters
		 genParVCov  = 0,	                          #@ Covariance matrix for generating fixed parameters
		 respEqn ,	                                  #@ Formula for creating response
		 respName = getEctdColName("Response"),	      #@ Response variable name
		 treatProp ,                                  #@ Proportions for sampling
		 treatOrder = FALSE,	                      #@ Logical flag: should allocations be assigned in order
		 conCovNames ,	                              #@ Continuous covariate names
		 conCovMean ,	                              #@ Continuous covariate means
		 conCovVCov ,	                              #@ Continuous covariate covariance matrix
		 conCovCrit = NULL,	                          #@ Continuous covariate acceptable range
		 conCovDigits = 3,	                          #@ Continuous covariate rounding digits
		 conCovMaxDraws = 100,	                      #@ Continuous covariate maximum draws
		 disCovNames ,	                              #@ Discrete covariate names
		 disCovVals ,	                              #@ Discrete covariate values
		 disCovProb ,	                              #@ Discrete covariate probabilities
		 disCovProbArray ,	                          #@ Array of probabilities for multivariate sampling
		 extCovNames ,	                              #@ Names for the continuous covariates
		 extCovFile ,	                              #@ File from which to import (including full or relative path)
		 extCovSubset ,	                              #@ Subset to apply to data
		 extCovRefCol ,	                              #@ Reference variable
		 extCovSameRow = TRUE,	                      #@ Logical flag: should covariates sampled be from the same row
		 extCovDataId = idCol,	                      #@ Subject variable name from file
		 timeCovNames ,	                              #@ Time-varying covariate names
		 timeCovMean ,	                              #@ Time-varying covariate means
		 timeCovVCov ,	                              #@ Time-varying covariate covariance
		 timeCovCrit = NULL,	                      #@ Time-varying covariate acceptable range   
		 genParCrit,	                              #@ Range of acceptable values for generated parameters
		 genParBtwNames ,	                          #@ Between subject effects to generate
		 genParBtwMean ,	                          #@ Means for generated between subject effects
		 genParBtwVCov ,	                          #@ Covariance matrix for generated between subject effects
		 genParErrStruc = "None",	                  #@ Function to map generated effects: Additive, Proportional or None
		 genParMaxDraws = 100,	                      #@ Maximum number of iterations to generate valid parameters
		 genParRangeTolerance = .5,					  #@ Proportion of subjects with "in range" parameters that we'd be happy proceeding with
		 extParFile ,	                              #@ File name for external parameter data to import
		 extParNames ,	                              #@ Names of parameters to import from external file
		 extParBtwNames ,	                          #@ Between subject effects variables to import from external file
		 extParBtwNums , 	                          #@ Integer mapping between random and fixed effects in imported parameter data
		 extParSubset = NULL,	                      #@ Subsets to be applied to imported parameter before sampling
		 extParCrit,	                              #@ Range of acceptable values for generated parameters
		 extParErrStruc = "None",	                  #@ Function to map effects from imported parameter data: Additive, Proportional or None
		 extParRefColData ,	                          #@ Reference column in imported parameter data
		 extParRefColName ,	                          #@ Reference column name from imported parameter data
		 extParDataId = idCol,                        #@ Subject variable in external parameter file
		 respInvLink,	                              #@ Inverse link function for the linear predictor
		 respDist = "Normal",	                      #@ Outcome response variable distribution
		 respVCov ,	                                  #@ Residual error (co)variance to apply to generated response
		 respErrStruc = "Additive",	                  #@ Function describing how to apply residual error to the generated response: Additive or Proportional
		 respCrit,	                                  #@ Range of acceptable values for created response
		 respDigits = 3,	                          #@ Number of digits to which to round the created response
		 mcarProp = 0,	                              #@ Proportion of observations to set to missing at random
		 mcarRule,	                                  #@ Rule to specify which observations of the data should be included for MCAR allocation
		 dropFun ,	                                  #@ User defined function to define criteria for subject dropout
		 dropFunExtraArgs = list(),	                  #@ Additional arguments to the dropout function
		 interimSubj ,	                              #@ Proportion of total subjects to be assigned to each interim analysis
		 interimMethod = "Sample",	                  #@ Method for creating interim variable: Sample or Proportion
         seed = .deriveFromMasterSeed(),              #@ random seed
		 idCol = getEctdColName("Subject"),	          #@ Subject variable name
		 doseCol = getEctdColName("Dose"),	          #@ Dose variable name
		 timeCol = getEctdColName("Time"),	          #@ Time variable name
		 trtCol = getEctdColName("Trt"),	          #@ Treatment variable name
		 parOmitFlag = getEctdColName("ParOmit"),	  #@ Parameter omit flag name
		 respOmitFlag = getEctdColName("RespOmit"),   #@ Response omit flag name
		 missingFlag = getEctdColName("Missing"),     #@ Missingness flag name
		 interimCol = getEctdColName("Interim"),      #@ Interim variable name
		 parBtwSuffix = ".Between",                   #@ Suffix for retained between subject effects variables
		 deleteCurrData = TRUE,                       #@ Should existing data be deleted before starting generation phase
		 covDiff = TRUE,                              #@ Should covariates differ between replicates
		 treatDiff = TRUE,                            #@ Should treatment allocation differ between replicates
		 workingPath = getwd()                        #@ Working directory from which to create data
){
 	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# generateData.R Mon Jul 02 15:14:30 BST 2007 @447 /Internet Time/
	#
	# Author: Richard
	###############################################################################
	# DESCRIPTION: High level function to generate simulated trial data
	# KEYWORDS: high, generate
	###############################################################################
	
	# Reset column names
	resetEctdColNames()
	
	# TODO: Better way of extracting default arguments to this function
	defNames <- c("treatSubj", "treatType", "respName", "treatOrder", "conCovCrit", "conCovDigits", "conCovMaxDraws", "extCovSameRow", "extCovDataId", "timeCovCrit", 
		"genParErrStruc", "respDist", "respErrStruc", "respDigits", "genParVCov", "mcarProp", "dropFunExtraArgs", "interimMethod", "seed", "idCol", "doseCol", "timeCol",
    	"trtCol", "parOmitFlag", "respOmitFlag", "missingFlag", "interimCol", "parBtwSuffix", "deleteCurrData", "covDiff", "treatDiff", "extParDataId")
  	callNames <- union(names(match.call())[-1], defNames)

	## Inner Function - Creates call lists from a vector of argument maps
	innerCallList <- function(Vec) {
		Vec <- Vec[Vec %in% callNames]
		if (length(Vec)) lapply(Vec, get, envir=parent.frame()) else list()
	}

	## Look at the values of treatSubj and subjects
	if (missing(subjects) & missing(treatSubj)) ectdStop("One of 'subjects' or 'treatSubj' must be supplied")
	if (missing(subjects)) subjects <- sum(treatSubj)
	if (length(subjects) != 1) ectdStop("The 'subjects' input must contain a single value")
	if (sum(treatSubj) != sum(subjects)) {
		if (sum(treatSubj) < sum(subjects)) {
			if ((sum(subjects) %% sum(treatSubj)) == 0) {
				if (length(treatSubj) == 1) treatSubj <- rep(treatSubj, sum(subjects) %/% sum(treatSubj))
			}
			else ectdStop("Values of 'subjects' and 'treatSubj' are incompatible.  'subjects' is not divisible by sum('treatSubj')")
		}
		else ectdStop("Values of 'subjects' and 'treatSubj' are incompatible. The value of 'subjects' is less than sum('treatSubj')")
	}

  	## Set Argument calling lists: matching of arguments
  	treatList <- innerCallList(c(doses = "treatDoses", times = "treatPeriod", type = "treatType", sequence = "treatSeq", doseCol = "doseCol", timeCol = "timeCol", trtCol = "trtCol"))
  	allocateList <- innerCallList(c(subjects = "treatSubj", prop = "treatProp", ordered = "treatOrder", idCol = "idCol", trtCol = "trtCol"))
  	covList <- innerCallList(c(subjects = "subjects", conNames = "conCovNames", conMean = "conCovMean", conCov = "conCovVCov", conRange = "conCovCrit", conDigits = "conCovDigits", 
    	conMaxDraws = "conCovMaxDraws", disNames = "disCovNames", disValues = "disCovVals", disProbs = "disCovProb", disProbArray = "disCovProbArray", extNames = "extCovNames", 
    	extFile = "extCovFile", extSubset = "extCovSubset", extRefCol = "extCovRefCol", extSameRow = "extCovSameRow", extDataId = "extCovDataId", idCol = "idCol", workingPath = "workingPath",
		timeNames = "timeCovNames", timeMean = "timeCovMean", timeCov = "timeCovVCov", timeRange = "timeCovCrit", timeCol = "timeCol", timePeriod = "treatPeriod"))
	parList <- innerCallList(c(subjects = "subjects", genNames = "genParNames", genFixedMean = "genParMean", 
		genFixedCov = "genParVCov", genRange = "genParCrit", genBetweenNames = "genParBtwNames", genBetweenMean = "genParBtwMean", genBetweenCov = "genParBtwVCov", 
		genErrStruc = "genParErrStruc", genMaxDraws = "genParMaxDraws", genParRangeTolerance = "genParRangeTolerance", extFile = "extParFile", extNames = "extParNames", extBetween = "extParBtwNames", extBetweenNums = "extParBtwNums", extSubset = "extParSubset", 
    	extRange = "extParCrit", extErrStruc = "extParErrStruc", extRefCol = "extParRefColData", extRefColName = "extParRefColName", extDataId = "extParDataId", suffix = "parBtwSuffix", idCol = "idCol", 
    	flagName = "parOmitFlag", workingPath = "workingPath"))
	respList <- innerCallList(c(equation = "respEqn", name = "respName", invLink = "respInvLink", distribution = "respDist", covariance = "respVCov", errStruc = "respErrStruc", 
		range = "respCrit", digits = "respDigits", flagName = "respOmitFlag"))
	mcarList <- innerCallList(c(prop = "mcarProp", rule = "mcarRule", flagName = "missingFlag" ))

	dropList <- innerCallList(c(dropFunc = "dropFun", idCol = "idCol", timeCol = "timeCol", flagName = "missingFlag"))
	interimList <- innerCallList(c(subjects = "subjects", proportion = "interimSubj", idCol = "idCol", interimCol = "interimCol", method = "interimMethod"))

	## Set directory structures
	if (deleteCurrData) removeDirectories("ReplicateData", workingPath = workingPath)
	createDirectories("ReplicateData", workingPath = workingPath)
  
	## Derive Treatment Data
	treatData <- do.call(createTreatments, treatList)
	allocateList$trts <- max(treatData[[trtCol]])                          

	## Allocate treatments if required
	if (!treatDiff) {
		allocData <- do.call(allocateTreatments, allocateList)
		allocData <- allocData [ rep(1:nrow(allocData), length = subjects), , drop = FALSE]
		allocData[[idCol]] <- 1:subjects
	}
	if (!covDiff) covData <- do.call(createCovariates, covList)
             
  	# Add buffer if we are adding to existing replicates in the directory
	buffer <- if (deleteCurrData) 0 
	else {
		allReps <- try(getReplicates(workingPath = workingPath), silent = TRUE)
		if (class(allReps) == "try-error") buffer <- 0
		else buffer <- max(allReps)
	}
  
	## Loop around replicates
	if (length(replicateN) == 1) replicateN <- 1:replicateN
	for (i in replicateN) {

		## Set low level component seeds
	    allocateList$seed <- seed + 1 * i
	    covList$seed      <- seed + 2 * i
	    parList$seed      <- seed + 3 * i
	    respList$seed     <- seed + 4 * i
	    mcarList$seed     <- seed + 5 * i
	    dropList$seed     <- seed + 6 * i
	    interimList$seed  <- seed + 7 * i	

	    ## Replicate Looping: Core Data Structure
   	 if (covDiff) covData <- do.call(createCovariates, covList)
   	 if (treatDiff) {
		 allocData <- do.call(allocateTreatments, allocateList)
		 allocData <- allocData [ rep(1:nrow(allocData), length = subjects), , drop = FALSE]
		 allocData[[idCol]] <- 1:subjects
	 }
	 
	 if (timeCol %in% names(covData)) bycov <- c(idCol, timeCol) else bycov <- idCol
	 coreData <- merge(merge(treatData, allocData, by=trtCol), covData, by=bycov)
	 sortBy <- c(idCol, trtCol, timeCol, doseCol)
	 sortBy <- sortBy [ sortBy %in% names(coreData) ]
	 if (length(sortBy)) coreData <- coreData [ do.call("order", coreData[sortBy]),,drop=FALSE]
	
   	 ## Replicate Looping: Parameters and Reponse
   	 if (!missing(extParRefColData) && length(extParRefColData) == 1 && is.character(extParRefColData) && !length(grep(",", extParRefColData))) {
   	   if (!length(grep(".refCol", extParRefColData))) extParRefColData <- paste(extParRefColData, ".refCol", sep="")
   	   if (extParRefColData %in% names(coreData)) parList$extRefCol <- coreData[[extParRefColData]]
   	   else parList <- parList[names(parList) != "extRefCol"]      
   	 }
    coreData <- merge(coreData, do.call(createParameters, parList), by=idCol)
    respList$data <- coreData
    respData <- do.call(createResponse, respList)
    if (is.data.frame(respData) && nrow(respData) == nrow(coreData)) coreData <- cbind(coreData, respData) else ectdStop("Cound not create response variable")

    ## Replicate Looping: Flags and Interims
    if (mcarProp > 0) {
      mcarList$data <- coreData
      coreData <- do.call(createMCAR, mcarList)
    }
    if (!missing(dropFun)) {
      dropList$data <- coreData
      coreData <- do.call(createDropout, c(dropList, dropFunExtraArgs))
    }

    if (!missing(interimSubj)) coreData <- merge(coreData, do.call(createInterims, interimList), by=idCol)

    ## Replicate Looping: Exporting Data
    writeData(coreData, i + buffer, dataType = "Replicate", workingPath = workingPath)

    .log( sprintf("gendata replicate %5d / %5d", i, length(replicateN)) )
  }
 
  # Set new default column names
  setEctdColName("Subject", idCol)
  setEctdColName("Dose", doseCol)
  setEctdColName("Time", timeCol)
  setEctdColName("Response", respName)
  setEctdColName("Trt", trtCol)
  setEctdColName("Interim", interimCol)
  setEctdColName("Missing", missingFlag)
  setEctdColName("ParOmit", parOmitFlag)
  setEctdColName("RespOmit", respOmitFlag)
  
  invisible()  
}



