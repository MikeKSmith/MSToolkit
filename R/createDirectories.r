#' Create replicate data, micro evaluation, and macro evaluation directories
#'
#' This function creates directories for holding data for the current scenario.
#' These directories will hold the replicate data, the micro evaluation data
#' and the macro evaluation data.
#'
#' Attempts to create directories named in character vector "dirNames" within
#' the "workingPath" directory.  No action is taken if method is "Internal"
#'
#' @param dirNames (Optional) A vector of directories to create under the
#' "workingPath" directory.  By default, directories "ReplicateData",
#' "MicroEvaluation" and "MacroEvaluation" are created
#' @param workingPath (Optional) The working directory in which to create
#' directories.  The current working directory is used by default
#' @param warn (Optional) A logical value.  Should warnings from the directory
#' creation be shown?  FALSE by default
#' @param method Data storage method (given by \link{getEctdDataMethod} by
#' default). MSToolkit can work with externalised data (as .csv or .RData files)
#' or internal data format, where the replicate data is stored as a list of
#' data frames in an internal environment (.ectdEnv$DataStore)
#' @return A logical vector the same length as "dirNames", indicating whether
#' or not the corresponding directories were successfully created.
#' @author Francisco Gochez
#' @keywords data
#' @examples
#'
#'   \dontrun{
#'     # Create 2 of the 3 directories
#'     createDirectories(dirNames = c("ReplicateData", "MicroEvaluation"))
#'   }
#'
#' @export
createDirectories <- function(
  dirNames = c("ReplicateData", "MicroEvaluation", "MacroEvaluation"), 		#@ A vector containing the full names of the directories to be created.
  workingPath = getwd(), 													#@ Directory in which to create directories
  warn = FALSE,          													#@ Should the dir.create function show warnings?
  method = getEctdDataMethod()   											#@ Data Storage Method to use
)
{
  ###############################################################################
  # Mango Solutions, Chippenham SN15 1BN 2009
  # createDirectories.R 03DEC09
  #
  # Author: Francisco Gochez
  ###############################################################################
  # DESCRIPTION: Tries to create named subdirectories for storing replicate, micro
  # evaluation and macro evaluation data.  Returns a logical vector representing
  # the success or failure of directory creation
  # KEYWORDS: IO
  ###############################################################################

  # Quit if the list is too short
  if(!length(dirNames)) ectdStop("No directories to create")

  # Match directory name against expected inputs
  dirNames <- match.arg(dirNames, several.ok = TRUE)

  # Match data storage method against recognised methods
  method <- match.arg(method, c("CSV", "RData", "Internal"))

  # Loop around if more than 1 directory
  if (length(dirNames) > 1) {
	 checkTrue <- c()
	 for (i in 1:length(dirNames)) checkTrue[i] <- createDirectories(dirNames[i], workingPath = workingPath, warn = warn, method = method)
	 return(checkTrue)
  }
  else {
	  # If we're only dealing with Micro/Macro, we should be using CSV method
	  if (substring(dirNames, 1, 1) == "M") method <- "CSV"

	  # Create physical directories
	  if (method %in% c("CSV", "RData")) {

		  # Create full directory paths
		  fullPath <- file.path(workingPath, dirNames)

		  # Create directories using the "dir.create" function
		  result <- dir.create(fullPath, showWarnings = warn)

		  # Log the creation of the directories
		  if (result) .log(paste("Created directory", fullPath))
		  return(result)
	  }
	  else return(TRUE)  # Don't need to create directories for "Internal" storage
  }
}
