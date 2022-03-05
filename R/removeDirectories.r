#' Remove directories
#'
#' This function removes directories containing data for the current scenario.
#'
#' dirNames must be a character vector.  The function will attempt to remove
#' each of the directories identified in the dirNames vector from within the
#' "workingPath" directory If method is "Internal", the DataStore is emptied
#' and no directories are modified
#'
#' @param dirNames (Optional) A vector of directories to remove from the
#' "workingPath" directory.  Default is to remove "ReplicateData",
#' "MicroEvaluation" and "MacroEvaluation" directories
#' @param workingPath (Optional) The working directory from which to remove the
#' directories.  By default, directories are removed from the current working
#' directory
#' @param method Data storage method (given by \link{getEctdDataMethod} by
#' default)
#' @return A logical vector the same length as the dirNames, indicating whether
#' or not the corresponding directories were successfully removed
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @keywords data
#' @examples
#'
#'   \dontrun{
#'     createDirectories(dirNames = c("ReplicateData", "MicroEvaluation"))
#'     removeDirectories(dirNames = c("ReplicateData", "MicroEvaluation"))
#'   }
#'
#' @export
removeDirectories <- function(
  dirNames = c("ReplicateData", "MicroEvaluation", "MacroEvaluation"),
                         #@ A vector containing the full names of the directories to be removed
  workingPath = getwd(), #@ Directory in which to remove directories
  method = getEctdDataMethod()   #@ Data Storage Method to use
) {
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # removeDirectories.R 20/06/2007 15:28:29 BST 2007 @445 /Internet Time/
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION: Tries to remove named subdirectories.  Returns a logical vector
  # representing the success or failure of directory removal
  # KEYWORDS: IO
  ###############################################################################

  # Quit if the list is too short
  if(!length(dirNames)) ectdStop("No directories to remove")

  # Match directory name against expected inputs
  dirNames <- match.arg(dirNames, several.ok = TRUE)

  # Match data storage method against recognised methods
  method <- match.arg(method, c("CSV", "RData", "Internal"))

  # Loop around if more than 1 directory
  if (length(dirNames) > 1) {

	  checkTrue <- c()

	  for (i in 1:length(dirNames)) checkTrue[i] <- removeDirectories(dirNames[i], workingPath = workingPath, method = method)

	  return(checkTrue)
  }
  else {

		# If we're only dealing with Micro/Macro, we should be using CSV method
		if (substring(dirNames, 1, 1) == "M") method <- "CSV"

		# Remove physical directories
		if (method %in% c("CSV", "RData")) {

			# Create full directory paths
			fullPath <- file.path(workingPath, dirNames)

			# Remove directories using the "unlink" function
			try(unlink(fullPath, recursive = TRUE))

			# Has the directory been removed?
			result <- !file.exists(fullPath)

			if (result) .log(paste("Removed directory", fullPath))

			return(result)

		}
  		else {
			.ectdEnv$DataStore <- NULL
			return(TRUE)
		}
	}
}
