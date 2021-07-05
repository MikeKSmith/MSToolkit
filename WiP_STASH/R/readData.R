#' Read scenario data
#' 
#' Reads a single element of replicate, macro evaluation or micro evaluation
#' data from the current scenario
#' 
#' 
#' @param dataNumber (Required) The number of the data entry you wish to read.
#' This number must be between 0 and 9999
#' @param dataType (Optional) The type of data you wish to read - either
#' "ReplicateData", "MacroEvaluation" or "MicroEvaluation".  By default,
#' "ReplicateData" is used
#' @param variables (Optional) The variables that must be in the data to
#' continue.  No variables are provided by default (so no variable check is
#' performed)
#' @param workingPath (Optional) The working path of the current scenario.  By
#' default, the current working directory is used
#' @param method (Optional) Data storage method (i.e. where to extract the data
#' from).  Given by \link{getEctdDataMethod} by default
#' @return readData will return a data frame containing the desired element of
#' data if it exists.
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso \code{\link{writeData}}
#' @keywords IO
#' @examples
#' 
#' 	\dontrun{
#'    readData(dataNumber = 10, dataType = "Macro")
#' 	 readData(dataNumber = 1, dataType = "Replicate")
#' 	 readData(dataNumber = 800, dataType = "Micro")
#'  }
#' 
readData <- function(
  dataNumber,               #@ The number of the data entry, should be between 1 and 9999
  dataType = c("ReplicateData", "MicroEvaluation", "MacroEvaluation"),        #@ String containing the type of data to be read ("Replicate", "Micro" or "Macro)
  variables = NULL,         #@ The variables we are expecting in the data
  workingPath = getwd(),     #@ Working directory
  method = getEctdDataMethod()
  )
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # readData.R Fri Jun 22 15:47:41 BST 2007 @418 /Internet Time/
  #
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION: Reads an individual data entry of the specified type (Replicate, Micro or Macro) and returns it as a data frame
  # KEYWORDS:IO
  ###############################################################################
{
	
  # Check data type
  dataType <- match.arg(dataType)
  if (dataType != "ReplicateData") method <- "CSV" 
	
  # Basic input checks
  .checkCharacter(dataType, method, workingPath)		# Check (character) inputs
  .checkNumeric(dataNumber)								# Check (numeric) input
  

  data <- switch(method, 
	"CSV" = {
		# Get the location of the file containing the specified data element
		fullPath <- .dataGetFullPath(dataNumber = dataNumber, dataType = dataType, workingPath = workingPath, method = method) 
		# Read the data
		try(.readAndCheckInputFile(fullPath, variables))
	}, 
	"RData" = {
		# Get the location of the file containing the specified data element
		fullPath <- .dataGetFullPath(dataNumber = dataNumber, dataType = dataType, workingPath = workingPath, method = method) 
		# Read the data
		try(.readFromRData(fullPath, variables))
	},
	"Internal" = {
		try(.ectdEnv$DataStore[[dataNumber]])
	}, 
	ectdStop("Did not recognise data storage method")
  )
	
  # Check and return structure
  if (!is.data.frame(data)) ectdStop(paste("Could not read data replicate", dataNumber))
  data
}
