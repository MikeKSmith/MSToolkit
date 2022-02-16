#' Create covariates
#' 
#' Function to create covariates. Discrete, continuous and/or from an external
#' file. This function acts as a wrapper for the functions :
#' \code{\link{createContinuousCovariates}},
#' \code{\link{createDiscreteCovariates}},
#' \code{\link{createDiscreteCovariates}} and
#' \code{\link{createTimeVaryingCovariates}}
#' 
#' According to the presence of each "names" argument, the function calls
#' lower-level functions to generate covariates.  For example, if
#' \code{conNames} is given, the function will attempt to generate covariates
#' from a continuous distribution using the
#' \code{\link{createContinuousCovariates}} function.
#' 
#' If no names is given, a data frame containing only the subject column will
#' be created.
#' 
#' If \code{\link{createTimeVaryingCovariates}} is invoked, the additional TIME
#' column will be included in the output data.
#' 
#' Arguments are systematically passed to lower-level function according to a
#' name convention.  For example, the \code{conRange} argument is passed to
#' \code{\link{createContinuousCovariates}} as the \code{range} argument, the
#' \code{extFile} is passed to the \code{\link{createExternalCovariates}} as
#' the \code{file} argument, ...
#' 
#' @param subjects (Required) Subjects for which to create covariates
#' @param conNames,conMean,conCov,conRange,conDigits,conMaxDraws (Optional)
#' Arguments for the \code{\link{createContinuousCovariates}} function.  The
#' \code{\link{createContinuousCovariates}} function is not called if these
#' arguments are not provided
#' @param disNames,disValues,disProbs,disProbArray (Optional) Arguments for the
#' \code{\link{createDiscreteCovariates}} function.  The
#' \code{\link{createDiscreteCovariates}} function is not called if these
#' arguments are not provided
#' @param extNames,extFile,extSubset,extRefCol,extSameRow,extDataId (Optional)
#' Arguments for the \code{\link{createExternalCovariates}} function.  The
#' \code{\link{createExternalCovariates}} function is not called if these
#' arguments are not provided
#' @param timeNames,timeMean,timeCov,timeRange,timeCol,timePeriod (Optional)
#' Arguments for the \code{\link{createTimeVaryingCovariates}} function.  The
#' \code{\link{createTimeVaryingCovariates}} function is not called if these
#' arguments are not provided
#' @param workingPath (Optional) Working directory from which to import
#' covariate data.  By default, the current working directory is used
#' @param idCol (Optional) Name of the subject column. Must be a valid R name
#' (See \code{\link{validNames}}) and not be found in anyone of
#' \code{conNames}, \code{extNames} or \code{disNames}.  "SUBJ" is used by
#' default
#' @param seed (Optional) Random seed to use by all the lower-level functions.
#' By default, this is derived from the current random seed
#' @return Data frame merging the results from the lower-level functions :
#' \code{\link{createContinuousCovariates}},
#' \code{\link{createDiscreteCovariates}},
#' \code{\link{createExternalCovariates}} and
#' \code{\link{createTimeVaryingCovariates}}
#' @section Warning: The function will generate an error if there is any
#' duplicated names between \code{conNames}, \code{extNames}, \code{disNames},
#' \code{timeNames} and \code{idCol}.
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso \code{\link{createContinuousCovariates}},
#' \code{\link{createDiscreteCovariates}},
#' \code{\link{createTimeVaryingCovariates}} and
#' \code{\link{createExternalCovariates}} for details about each source of
#' covariate and examples.
#' @keywords datagen IO
#' @examples
#' 
#' 
#'   ## unit tests for the covariates component of the MSToolkit package 
#'   \dontrun{
#'     file.show( system.file( "Runit", "runit.data.covariates.R", package = "MSToolkit") )
#'   
#'     wPath <- system.file( "Runit", "data", "createCovariates", package = "MSToolkit") 
#'     dAll <- createCovariates( 30, 
#'       conNames = "X,Y", conMean = "0,0" , conCov = "1,0,1", conRange = "-1<X<1",  # continuous
#'       disNames = "P1,P2", disValues = "1,2#3,5,6" , disProbs = ".5,.5#.3,.3,.4",  # discrete
#'       extNames = "X1", extFile = "testCovariates.csv", workingPath = wPath )      # external  
#'     
#'     dAllwithtime <- createCovariates( 30, 
#'       conNames = "X,Y", conMean = "0,0" , conCov = "1,0,1", conRange = "-1<X<1", # continuous
#'       disNames = "P1,P2", disValues = "1,2#3,5,6" , disProbs = ".5,.5#.3,.3,.4", # discrete
#'       timeNames = "T1,T2", timeMean = list(1:3, 1:3), timeCov = list(1, 1:3),    # time-varying
#'       timePeriod = 1:3, extNames = "X1", extFile = "testCovariates.csv",         # external 
#'       workingPath = wPath )        
#'  }
#' 
createCovariates <- function(
  subjects,     #@ Subjects for which to create covariates

  ## arguments for the `createContinuousCovariates` function
  conNames=NULL,#@ Continuous covariate names
  conMean,      #@ Continuous covariate means
  conCov,       #@ Continuous covariate covariance matrix
  conRange=NULL,#@ Continuous covariate acceptable range
  conDigits,    #@ Continuous covariate rounding digits
  conMaxDraws=100,#@ Continuous covariate maximum draws

  ## arguments for the `createDiscreteCovariates` function
  disNames=NULL,#@ Discrete covariate names
  disValues,    #@ Discrete covariate values
  disProbs,     #@ Discrete covariate probabilities
  disProbArray, #@ Array of probabilities for multivariate sampling

  ##arguments for the `createExternalCovariates` function
  extNames=NULL,#@ Names for the continuous covariates
  extFile,      #@ File from which to import (including full or relative path)
  extSubset,    #@ Subset to apply to data
  extRefCol,    #@ Reference variable
  extSameRow=TRUE,   #@ Logical flag: should covariates sampled be from the same row
  extDataId=idCol, #@ Subject variable name from file
  workingPath = getwd(), #@ Working directory

  ## arguments for the `createTimeVaryingCovariates` function
  timeNames=NULL,
  timeMean,
  timeCov,
  timeRange=NULL,
  timeCol = getEctdColName("Time"),
  timePeriod,

  ## common args
  idCol = getEctdColName("Subject"),  #@ Subject variable name for return data
  seed=.deriveFromMasterSeed() #@ random seed
){
 	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# createCovariates.R Fri Jun 01 10:44:40 BST 2007 @447 /Internet Time/
	#
	# Author: Romain
	###############################################################################
	# DESCRIPTION: covariate component, wrapper for the functions:
  #              createContinuousCovariates, createExtenalCovariates,
  #              createDiscreteCovariates
  # KEYWORDS: datagen, component:covariate
	###############################################################################

  set.seed( seed )

  subjects <- .expandSubjects( subjects )
  idCol    <- parseCharInput( idCol, convertToNumeric = FALSE, expected = 1, valid = TRUE)
  timeCol  <- parseCharInput( timeCol, convertToNumeric = FALSE, expected = 1, valid = TRUE)

  conNames <- parseCharInput( conNames, convertToNumeric = FALSE, checkdup = TRUE )
  extNames <- parseCharInput( extNames, convertToNumeric = FALSE, checkdup = TRUE )
  disNames <- parseCharInput( disNames, convertToNumeric = FALSE, checkdup = TRUE )
  timeNames <- parseCharInput( timeNames, convertToNumeric = FALSE, checkdup = TRUE )
  if( any(duplicated(c(conNames, extNames, disNames, timeNames))))
    ectdStop("duplicated names in `conNames`, `extNames`, `disNames`, `timeNames`")

  ## calling the createContinuousCovariates function
  dataList <- NULL
  dataList[[idCol]] <- .eval( "data.frame( $idCol = subjects)" )

  dataList$continuous <- if( !is.null(conNames) ){
    conArgs <- list(
      subjects = subjects,  names    = conNames,
      idCol    = idCol,  seed     = seed,
      range    = conRange, maxDraws = conMaxDraws,
      includeIDCol = FALSE)
    if(!missing(conMean))    conArgs$mean       <- conMean
    if(!missing(conCov))     conArgs$covariance <- conCov
    if(!missing(conDigits))  conArgs$digits     <- conDigits

    do.call( createContinuousCovariates, conArgs)
  }

  ## calling the createExternalCovariates function
  dataList$external <- if( !is.null(extNames) ){
    extArgs <- list( subjects = subjects, names = extNames, idCol = idCol,
      seed = seed, sameRow = extSameRow, dataId = extDataId,
      includeIDCol = FALSE, workingPath = workingPath )
    if(!missing(extFile) )    extArgs$file   <- extFile
    if(!missing(extSubset) )  extArgs$subset <- extSubset
    if(!missing(extRefCol) )  extArgs$refCol <- extRefCol

    do.call( createExternalCovariates, extArgs)
  }

  ## calling the createDiscreteCovariates function
  probCall <- !missing(disProbArray) && length(disProbArray)
  dataList$discrete <- if( !is.null(disNames) | probCall){
    disArgs <- list( subjects = subjects, idCol = idCol, seed = seed, includeIDCol = FALSE )
		if( !missing(disNames    )) disArgs$names     <- disNames
		if( !missing(disValues   )) disArgs$values    <- disValues
		if( !missing(disProbs    )) disArgs$probs     <- disProbs
		if( !missing(disProbArray)) disArgs$probArray <- disProbArray
		do.call( createDiscreteCovariates, disArgs)
  }

  names( dataList )  <- NULL
  out <- do.call(data.frame, dataList[!sapply(dataList, is.null)]  )

  out.time <- if( !is.null(timeNames) ){
	  timeArgs <- list(
			  subjects = subjects,  names    = timeNames,
			  idCol    = idCol,  seed     = seed,
			  range    = timeRange, maxDraws = conMaxDraws,
			  timeCol = timeCol)
	  if(!missing(timeMean))    timeArgs$mean       <- timeMean
	  if(!missing(timeCov))     timeArgs$covariance <- timeCov
	  if(!missing(conDigits))  timeArgs$digits     <- conDigits
	  if(!missing(timePeriod))  timeArgs$treatPeriod     <- timePeriod

	  do.call( createTimeVaryingCovariates, timeArgs)
  }

  if (!is.null(out.time)) out <- merge(out.time, out)

  out

}



