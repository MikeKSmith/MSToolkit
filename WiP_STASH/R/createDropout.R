#' Adds dropout to a simulated dataset
#' 
#' Applies a given dropout function to a simulated trial dataset, in order to
#' create a "missing" variable representing subject dropout
#' 
#' This function will accept (and check) a "dropout" function, which is to
#' applied to the data.  This function must return a boolean vector.  The
#' dropout function supplied will be applied to the data provided (possibly
#' with extra arguments supplied with the ellipses).  Once a boolean vector is
#' returned, the function will ensure dropout is "retained" over time within
#' subject (so if a subject drops out at visit 1, he/she is also missing at
#' visits 2, 3 etc.).  This "retained" boolean vector is added to the dataset
#' (or used to enhance an existing "missing" flag) and the updated data is
#' returned.
#' 
#' @param data (Required) Data frame to which to add a "missingness" flag
#' @param dropFunc (Required) Drop out function to apply to the data.  The
#' function must return a vector of booleans
#' @param seed (Optional) Random generation seed.  By default, the current
#' random seed is used
#' @param idCol (Optional) The name of the subject varuable in the data.
#' "SUBJ" by default
#' @param timeCol (Optional) The name of the time column in the data.  "TIME"
#' by default
#' @param flagName (Optional) The name of the missing variable to create in the
#' data.  "MISSING" by default
#' @param \dots (Optional) Extra arguments to be passed directly to the dropout
#' function.  No arguments are passed by default
#' @return An updated data frame with added/updated "missing" column
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso \code{\link{checkDropOutFun}} and \code{\link{createMCAR}}
#' @keywords datagen
#' @examples
#' 
#' \dontrun{
#' 
#' dFun <- function(data, prop) sample(0:1, nrow(data), replace=TRUE, prob=c(1-prop, prop))
#' createDropout(data=myDf, dropFunc=dFun, prop=.05)
#' #     SUBJ TIME MISSING
#' #  1     1    1       0
#' #  2     1    2       0
#' #  3     1    3       0
#' #  4     1    4       0
#' #  5     1    5       0
#' #  6     2    1       0
#' #  7     2    2       0
#' #  8     2    3       1
#' #  9     2    4       1
#' #  10    2    5       1
#' #  11    3    1       0
#' #  12    3    2       1
#' #  13    3    3       1
#' #  14    3    4       1
#' #  15    3    5       1
#' }
#' 
createDropout <- function(
  data,                             #@ Data Structure to which to add missing flag
  dropFunc,                         #@ Drop out function
  seed = .deriveFromMasterSeed( ),  #@ Random seed
  idCol = getEctdColName("Subject"),                     #@ name of the subject varuable in the data
  timeCol = getEctdColName("Time"),                 #@ name of the time column
  flagName = getEctdColName("Missing"),             #@ name of the missing variable to create
  ...                               #@ to pass to dropFunc
){
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # createDropout.R Tue Jun 19 11:01:32 BST 2007 @459 /Internet Time/
  #
  # Author: Romain
  ###############################################################################
  # DESCRIPTION: add a misisng flag according to a user-supplied dropout function
  # KEYWORDS: component:data:missingness
  ###############################################################################

	set.seed( seed )
	flagName <- parseCharInput( flagName, expected = 1, convertToNumeric = FALSE, valid = TRUE)
	idCol    <- parseCharInput( idCol, expected = 1, convertToNumeric = FALSE, valid = TRUE)

	if( !missing(timeCol)){
		timeCol  <- parseCharInput( timeCol, expected = 1, convertToNumeric = FALSE, valid = TRUE)
		if (!(timeCol %in% names(data))) ectdStop("The `$timeCol` column is not in the supplied data")
	}
	if (!(idCol %in% names(data))) ectdStop("The `$idCol` column is not in the suppplied dataset")

	## Ensure the dropout function is valid
	checkDropOutFun( dropFunc, data, ... )

	### call the user-supplied function to get the missing flag
	missingFlag <- try( dropFunc( data, ... ), silent = TRUE )
	if (class(missingFlag) == "try-error") ectdStop("Error occuring when calling the dropout function: $missingFlag" )

	## Retain variable if time variable present
	if (timeCol %in% names(data) & any(missingFlag > 0)) {
		isMiss <- missingFlag > 0
    	minMiss <- tapply(data[isMiss, timeCol], data[isMiss, idCol], min)
    	missMatch <- minMiss[as.character(data[[idCol]])]
    	quickTest <- !is.na(missMatch) & data[[timeCol]] > missMatch
    	missingFlag <- as.numeric(quickTest | missingFlag)
	}

	## Add or modify the missing flag in the data set
	data[[ flagName ]] <- if( flagName %in% names(data) ){
		1 * ( data[[ flagName ]] | missingFlag )
	}
	else missingFlag
	data
}
