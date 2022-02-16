#' Create covariates by sampling from an external file
#'
#' Create covariates by sampling from an external file.
#'
#' The sampling is always done with replacement.
#'
#' The \code{refCol} setting is typically used with the parameter component,
#' see \code{\link{createParameters}} or \code{\link{createExternalParameters}}
#' to maintain consistency between imported covariates and imported parameters.
#'
#' @param subjects (Required) Number of subjects for which to sample sets of
#' external covariate values
#' @param names (Required) Names of the covariates to use from the \code{file}
#' @param file (Required) Input file name.  This should be either a valid csv
#' file or a NONMEM data file containing all the variables given by
#' \code{names}, \code{idCol}
#' @param sameRow (Optional) A logical value).  Should all the covariates be
#' sampled from the same rows or should the sampling be done independantly for
#' each covariate.  Using \code{sameRow = TRUE} would maintain the multivariate
#' structure of the imported dataset and is faster.  TRUE by default
#' @param subset (Optional) Any subset to be performed on the imported dataset
#' before doing any sampling. The subset is parsed by the
#' \code{\link{parseRangeCode}} function.  No subsetting is performed by
#' default
#' @param refCol (Optional) The reference column in the data file.  If given,
#' the output dataset will also contain an additional column indicating the
#' origin of each row from the original dataset. This option is not compatible
#' with \code{sameRow = FALSE}.  By default, reference variables are not used
#' @param dataId (Optional) The subject variable in the input dataset, equal to
#' \code{idCol} by default.  By default, it will be the same as "idCol"
#' @param idCol (Optional) The subject variable in the output dataset.  "SUBJ"
#' by default
#' @param percent (Optional) When a subset is performed on the input data, if
#' the number of rows remaining in the dataset after subset is less than
#' \code{percent} \%, the function will issue a warning.  By default, 20\% is
#' used
#' @param seed (Optional) Random generator seed to use.  The current random
#' seed is used by default
#' @param includeIDCol (Optional) A logical value.  Should the subject variable
#' be included in the output dataset?  When \code{createCovariates} calls this
#' function, it does not need the subject variable.  TRUE by default
#' @param refColSuffix (Optional) The suffix to use when creating the
#' \code{refCol} variable.  If the \code{refCol} variable is "SUBJ", then in
#' the output dataset it will be created as "SUBJ" suffixed with
#' "refColSuffix".  By default, "refCol" is used as the suffix
#' @param workingPath (Optional) Working path from which to import covariate
#' file.  By default, the current working directory is used
#' @return A data frame containing the imported variables, and possibly a
#' reference variable.
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso \code{\link{createContinuousCovariates}},
#' \code{\link{createDiscreteCovariates}}, and \code{\link{createCovariates}}
#' @keywords datagen IO
#' @examples
#' \dontrun{
#'
#'   # an example file from the unit tests of the MSToolkit package
#'   wPath <- system.file( "Runit", "data", "createCovariates", package = "MSToolkit" )
#'
#'   # sample 20 subjects from the example file
#'   dat <- createExternalCovariates( 20, names = "X1",
#'     subset = c(".7 < X1 < .8", "-1 <= X2 <= 1"),
#'     file = "testCovariates.csv", workingPath = wPath )
#'   print( dat )
#'
#'   # maintaining the origin of each row
#'   dat <- createExternalCovariates( 20, names = "X1, X2",
#'     subset = c(".7 < X1 < .8", "-1 <= X2 <= 1"),
#'     file = "testCovariates.csv", workingPath = wPath, refCol = "ID" )
#'   print( dat )
#'
#' }
createExternalCovariates <- function(
  subjects,       #@ Subjects for which to create covariates
  names,          #@ Names for the continuous covariates
  file,           #@ File from which to import (including full or relative path)
  sameRow = TRUE, #@
  subset = NULL,  #@ Subset to apply to the data
  refCol = NULL,  #@ Parameter reference variable in dataset
  dataId = idCol, #@ Subject variable in data
  idCol = getEctdColName("Subject"),   #@ Subject variable name
  percent = 20,   #@ Percentage of values to use for checking rows in dataset vs samples to extract
  seed = .deriveFromMasterSeed(), # Random seed
  includeIDCol = TRUE,
  refColSuffix = "refCol",
  workingPath = getwd()
){
 	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# createExternalCovariates.R Fri Jun 01 10:43:38 BST 2007 @446 /Internet Time/
	#
	# Author:
	###############################################################################
	# DESCRIPTION: import a set of covariates from a file
  # KEYWORDS: datagen, io, component:covariate
	###############################################################################

  set.seed( seed )
  subjects <- .expandSubjects( subjects )
  nSubjects <- get("nSubjects")
  names    <- parseCharInput(names, convertToNumeric = FALSE, checkdup = TRUE)
  subset   <- parseRangeCode( subset )
  validNames( idCol, dataId, names)
  if(!is.null(refCol)) validNames( refCol )
  percent <- parseCharInput( percent, expected = 1, convertToNumeric = TRUE )
  if( percent < 0 || percent > 100)
    ectdStop("`percent` should be between 0 and 100")

  iData <- .readAndCheckInputFile( file.path(workingPath, file), c(dataId, names) )
  if(!is.null(refCol) && refCol %!in% names(iData) )
    ectdStop("There is no column `$refCol` in the dataset `$file`")
  if(!is.null(refCol) && !sameRow )
    ectdStop("sameRow = FALSE is not compatible with the use of refCol")
  if( !is.null(subset)) iData <- .applyDataSubset(iData, subset)

  # taking the first value for each ID
  iData <- iData[ !duplicated(iData[[dataId]]), ,drop = FALSE]

  if( nrow(iData) < percent * nSubjects / 100  )
    ectdWarning("Less than $percent % of lines in the dataset compared to the number of subjects to sample")

  if(sameRow){
    idx <- sample( nrow(iData), replace = TRUE, size = nSubjects )
    out <- iData[ idx, c(names, refCol), drop = FALSE ]
    if(!is.null(refCol)) names(out)[length(out)] <- names(out)[length(out)] %.% '.' %.% refColSuffix

    ## add the ID variable
    if( includeIDCol ) out <- .eval( "data.frame( $idCol = subjects, out)" )

  }  else {
    out <- as.data.frame( matrix(NA, nrow = nSubjects, ncol = length(names)+includeIDCol ) )
    if( includeIDCol ){
      names(out) <- c( idCol, names)
      out[[idCol]] <- subjects
    } else {
      names(out) <- names
    }
    for( nm in names) out[[ nm ]] <- sample( iData[[nm]], size= nSubjects, replace = TRUE)
  }
  rownames( out ) <- 1:nSubjects
  out
}

