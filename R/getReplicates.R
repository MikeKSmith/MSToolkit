#' Trial replicates vector
#'
#' Get a vector of available trial replicates, based on a specified data
#' storage method
#'
#' This function looks for existing replicates based on the method, directories
#' and file prefix given.
#'
#' If method is "CSV", the function looks for files starting with "prefix" and
#' ending in ".csv" within the "path" subdirectory of "workingPath" Similarly,
#' if method is "RData", the function looks for files starting with "prefix"
#' and ending in ".rdata" in the same location If method is "Internal", the
#' function looks for data frames found in the internal data storage location
#' (.ectdEnv$DataStore)
#'
#' The function will return a vector of replicate numbers, or fail if no
#' replicates are found
#'
#' @param path The subdirectory (within "workingPath") containing the
#' replicates files ('CSV' and 'RData' methods only)
#' @param prefix Prefix to used when looking for simulated data files
#' @param method Data storage method in use, either "CSV", "RData" or "Internal
#' (given by \link{getEctdDataMethod} by default)
#' @param workingPath Root path for simulated data (default is working
#' directory)
#' @return A numeric vector of replicate values
#' @keywords Replicates
#' @examples
#'
#'
#'   \dontrun{
#' 	 getReplicates()
#'   }
#'
"getReplicates" <- function(
		path = "ReplicateData", 			#@ Path within which replicates are stored
		prefix = "replicate", 				#@ Prefix for files in the directory
		method = getEctdDataMethod(),		#@ Data storage method
		workingPath = getwd()				#@ Working directory
) {
	.checkCharacter(path, prefix, method, workingPath)
	switch(method,
			"CSV" = {
				path <- file.path(workingPath, path)
				if (!file.exists(path)) ectdStop(
				  paste("Could not find path \"", path, "\"", sep="")
				  )
				searchPattern <- paste("^", prefix, ".+[0-9]+\\.[cC][sS][vV]$", sep="")
				fileNames <- casefold(list.files(path, pattern=searchPattern))
				fileNames <- gsub("\\.csv", "", gsub(casefold(prefix), "", fileNames))
				if (!length(fileNames)) ectdStop(
				  paste("No files to read from directory", path)
				  )
				replicates <- try(as.numeric(fileNames))
				if (class(replicates) == "try-error") ectdStop(
				  paste("File names in directory", path, "of wrong format")
				  )
			},
			"RData" = {
				path <- file.path(workingPath, path)
				if (!file.exists(path)) ectdStop(
				  paste("Could not find path \"", path, "\"", sep="")
				  )
				searchPattern <- paste("^", prefix, ".+[0-9]+\\.[rR][dD][aA][tT][aA]$",
				                       sep="")
				fileNames <- casefold(list.files(path, pattern=searchPattern))
				fileNames <- gsub("\\.rdata", "", gsub(casefold(prefix), "", fileNames))
				if (!length(fileNames)) ectdStop(
				  paste("No files to read from directory", path)
				  )
				replicates <- try(as.numeric(fileNames))
				if (class(replicates) == "try-error") ectdStop(
				  paste("File names in directory", path, "of wrong format")
				  )
			},
			"Internal" = {
				if (!length(.ectdEnv$DataStore)) ectdStop(
				  "No data to read from internal data store")
				replicates <- 1:length(.ectdEnv$DataStore)
			},
			ectdStop(
			  paste("Data storage method \"", method, "\" not recognised", sep=""))
	)
	replicates
}
