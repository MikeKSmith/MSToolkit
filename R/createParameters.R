#' Creates parameters for subjects in each replicate
#'
#' Creates simulated fixed and between subject parameters for subjects in each replicate
#'
#' @param subjects Subjects for which to create parameters
#' @param genNames Names of fixed effects to generate
#' @param genFixedMean Means for generating fixed parameters
#' @param genFixedCov Covariance Matrix for generating fixed parameter. \emph{0} by default.
#' @param genRange Range of Acceptable values for derived parameters
#' @param genBetweenNames Between subjects effects to generate
#' @param genBetweenMean Means for generated between subject effects
#' @param genBetweenCov Between subject effects covariance.
#' @param genErrStruc   The function to map effects: \emph{Additive}, \emph{Proportional}, \emph{Log-Normal} or \emph{None}. The default is \emph{"None"}.
#' @param genMaxDraws The maximum number of draws. The default value is \emph{10}.
#' @param genParRangeTolerance The proportion of subjects with "in range" parameters with. The default value is \emph{0.5}.
#'
#' @param extFile The external file name for data to import.
#' @param extNames Names of parameters to import (refers to variables in the input data)
#' @param extBetween Between subject effects variables in the data
#' @param extBetweenNums The integer mapping between random and fixed effects.
#' @param extSubset The subset to be applied to data before sampling.
#' @param extRange The range of Acceptable values for derived parameters.
#' @param extErrStruc Function to map between subject effects: \emph{Additive}, \emph{Proportional}, \emph{Log-Normal} or \emph{None}.
#' The default is \emph{"None"}.
#' @param extRefCol The column of reference data.
#' @param extRefColName The column name in data for referenced sampling.
#' @param extRefColSuffix Suffix to add to reference variable in the data.
#' @param extIndEffects Individual effects flag
#' @param extDataId The external subject variable name. By default,
#' this is taken to be the same as the "idCol" input.
#' @param workingPath Working path from which to import covariate file. By default, the working path will be used
#'
#' @param suffix  Suffix for retained between subject effects. The default is \emph{".Between"}.
#' @param idCol Subject variable name. The default is \emph{getEctdColName("Subject")}.
#' @param seed Random seed. The default is \emph{.deriveFromMasterSeed()}.
#' @param flagName Name for omit flag. The default is \emph{getEctdColName("ParOmit")}.
#'
#' @export
createParameters <- function(
    subjects,              #@ subjects for which to create parameters

    ## arguments for the `createNormalParameters` function
    genNames,              #@ Names of fixed effects to generate
    genFixedMean,          #@ Means for generating fixed parameters
    genFixedCov = 0,       #@ Covariance Matrix for generating fixed parameter
    genRange,              #@ Range of Acceptable values for derived parameters
    genBetweenNames,       #@ Between subjects effects to generate
    genBetweenMean,        #@ Means for generated bw effects
    genBetweenCov,         #@ bw covariance
    genErrStruc = "None",  #@ how to map effects
    genMaxDraws = 10,      #@ maximum number of draws
    genParRangeTolerance = .5,  ## Proportion of subjects with "in range" parameters with which we'd be happy proceeding

    ## arguments for the `createExternalParameters` function
    extFile,               #@ file name for data to import
    extNames,              #@ Names of parameters to import
    extBetween,            #@ bw subject effect variables in the data
    extBetweenNums,        #@ integer mapping  between random and fixed effects
    extSubset,             #@ subset to be applied to data before sampling
    extRange,              #@ Range of Acceptable values for derived parameters
    extErrStruc = "None",  #@ function to map effects
    extRefCol,             #@ Column of reference data
    extRefColName,         #@ Reference column name
    extRefColSuffix,       #@ Reference column name
    extIndEffects,         #@ individual effects flag
    extDataId = idCol,     #@ External subject variable name
    workingPath = getwd(), #@ Working directory

    ## arguments for both
    suffix = ".Between",   #@ suffix for retained between subject effects
    idCol  = getEctdColName("Subject"),         #@ Subject variable name
    seed = .deriveFromMasterSeed( ) , #@ Random seed
    flagName = getEctdColName("ParOmit")   #@ name for omit flag
)
{
  ##############################################################################
  # Mango Solutions, Chippenham SN15 1BN 2009
  # createParameters.R Thu Jun 21 16:54:02 BST 2007 @704 /Internet Time/
  #
  # Author: Romain/Rich P
  ###############################################################################
  # DESCRIPTION: create parameters wrapper
  # KEYWORDS: datagen, component:data:parameter
  ##############################################################################

  subjects <- .expandSubjects( subjects )

  idCol    <- parseCharInput( idCol, expected = 1, convertToNumeric = FALSE, valid = TRUE)
  flagName <- parseCharInput( flagName, expected = 1, convertToNumeric = FALSE, valid = TRUE)

  test1 <- !missing(genNames) && !missing(genFixedMean)
  test2 <- !missing(extFile)  && !missing(extNames)

  if (test1) genNames <- parseCharInput( genNames , convertToNumeric = FALSE, checkdup = TRUE)
  if (test2) extNames <- parseCharInput( extNames , convertToNumeric = FALSE, checkdup = TRUE)
  if(test1 & test2 && any(genNames %in% extNames) ) ectdStop( "Duplicated names between `genNames` and `extNames`")

  if( !test1 && !test2){
    out <- .eval( "data.frame( $idCol = subjects, $flagName = rep(0, length(subjects)) )" )
    return( out )
  }
  if( test1 ){
    ## generate parameters from a MVN distribution using createNormalParameters

    ## build the argument list
    argsNP <- list( subjects = subjects, idCol = idCol,
                    seed = seed, flagName = flagName, suffix = suffix,
                    errStruc = genErrStruc, covariance = genFixedCov, parRangeTolerance = genParRangeTolerance  )
    ## handle missing arguments
    if(!missing(genNames       )) argsNP$names      <- genNames
    if(!missing(genFixedMean   )) argsNP$mean       <- genFixedMean
    if(!missing(genRange  ))      argsNP$range      <- genRange
    if(!missing(genBetweenNames)) argsNP$betNames   <- genBetweenNames
    if(!missing(genBetweenMean )) argsNP$betMean    <- genBetweenMean
    if(!missing(genBetweenCov  )) argsNP$betCov     <- genBetweenCov
    if(!missing(genMaxDraws    )) argsNP$maxDraws   <- genMaxDraws

    ## call the createNormalParameters function and check its output
    genData <- try( do.call(createNormalParameters, argsNP), silent = TRUE )
    if( class(genData) == "try-error") ectdStop( "Errors when building data from normal distribution\n\t$genData"  )
    if( !is.data.frame(genData)) ectdStop( "The dataset generated by `createNormalParameters` is not a data frame")
    if( nrow(genData) != length(subjects) ) ectdStop( "The number of lines in the dataset does not match the number of subjects requested")
  }

  if( test2 ){
    ## generate parameters by sampling from a file using `createExternalParameters`

    ## build the argument list
    argsEX <- list( subjects = subjects, idCol = idCol, seed = seed, flagName = flagName,
                    errStruc = extErrStruc, file = extFile, names = extNames, workingPath = workingPath, dataId = extDataId )
    ## handle missing arguments
    if( !missing(extBetween    )) argsEX$betNames    <- extBetween
    if( !missing(extBetweenNums)) argsEX$betNums     <- extBetweenNums
    if( !missing(extSubset     )) argsEX$subset      <- extSubset
    if( !missing(extRange      )) argsNP$range       <- extRange
    if( !missing(extRefCol     )) argsEX$refCol      <- extRefCol
    if( !missing(extRefColName )) argsEX$refColName  <- extRefColName
    if( !missing(extRefColSuffix)) argsEX$refColSuffix  <- extRefColSuffix
    if( !missing(extIndEffects )) argsEX$indEffects  <- extIndEffects

    ## call the function and check consistency of the output
    extData <- try( do.call( createExternalParameters, argsEX), silent =  TRUE)
    if( class(extData) == "try-error") ectdStop( "Errors when importing data from file \n\t $extFile" )
    if( !is.data.frame(extData)) ectdStop( "The dataset generated by `createExternalParameters` is not a data frame")
    if( nrow(extData) != length(subjects) ) ectdStop( "The number of lines in the dataset does not match the number of subjects requested")
  }

  ## build the output data
  out <- switch( test1 + 2*test2,
                 genData,                  # (1) only test1 is TRUE ->  normal data
                 extData,                  # (2) only test2 is TRUE ->  external data
                 {                         # (3) merge genData and extData
                   namesGD <- names(genData)
                   namesEX <- names(extData)
                   data.frame(
                     genData[,namesGD != flagName, drop = FALSE],  # all data from genData but the paromit
                     extData[,namesEX %!in% c(idCol, flagName) , drop = FALSE],   # all data from extData but the id and flag
                     1 * ( extData[,flagName,drop=FALSE] | genData[,flagName,drop=FALSE] ) ) # omit from gen or ext
                 })
  out
}