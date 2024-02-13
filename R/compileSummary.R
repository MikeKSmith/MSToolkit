#' Compile all Micro or Macro evaluation data
#'
#' This function reads all of the micro evaluation or macro evaluation data and
#' amalgamates it into a single data frame which has an additional column
#' indicating which replicate each row belongs to.  This data frame is then
#' written to "microSummary.csv" or "macroSummary.csv".
#'
#' Builds the path to each existing Micro or Macro file to be compiled.  Reads
#' the data for each as text, then paste on a Replicate column name to each
#' text section imported.  Write out a "full" dataset as a compilated of the
#' seperate datasets found
#'
#' @param dataType (Optional) A string that indicates which of the two data
#' types should be read. Must be either "MicroEvaluation" or "MacroEvaluation".
#' Partial matching is used here.  See \code{\link{match.arg}}.  Default is
#' "MicroEvaluation"
#' @param replicates (Optional) Which replicates should be compiled. By
#' default, the function will compile all available replicate data at the time
#' @param prefix Prefix to use when searching for files to compile
#' @param replicateCol Replicate column name (given by \link{getEctdColName} by
#' default)
#' @param workingPath (Optional) The working directory to be used as the root
#' for the "Micro" and "Macro" evaluation directories.  Also the directory in
#' which the compiled summary files will be created.  The current working
#' directory is used by default
#' @return CompileSummary does not return a value.
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @keywords IO
#' @examples
#'
#'   \dontrun{
#'     # a directory MicroEvaluation need to be there and be full
#'     # of files matching the pattern micro[0-9]{4}.csv
#'     compileSummary(dataType = "Micro")
#'
#'     # a directory MacroEvaluation need to be there and be full
#'     # of files matching the pattern macro[0-9]{4}.csv
#'     compileSummary(dataType = "Macro")
#'
#'   }
#' @export
"compileSummary" <- function (
		dataType = c("MicroEvaluation", "MacroEvaluation"),
		replicates = NULL,
		prefix = switch(dataType, MicroEvaluation = "micro", MacroEvaluation = "macro"),
		replicateCol = getEctdColName("Replicate"),
		workingPath = getwd()
)
{
	# Check the inputs
	dataType <- try(match.arg(dataType), silent = TRUE)
	if (class(dataType) == "try-error") ectdStop("Invalid data type: $dataType")
	prefix <- casefold(substring(dataType, 1, 5))
	if (!file.exists(file.path(workingPath, dataType))) ectdStop("directory $dataType unavailable under $workingPath")
	if (!length(repfiles <- dir(file.path(workingPath, dataType), pattern = "m[ia]cro[[:digit:]]{4}\\.csv"))) ectdStop("no data files under $workingPath/$dataType")
	if (!is.null(replicates) && !all(out <- file.exists(targetfiles <- file.path(workingPath, dataType, sprintf("%s%04d.csv", prefix, replicates)))))
		ectdStop(paste("Impossible to compile the data, the following files are missing: ", paste(targetfiles[!out], collapse = "\n\t"), sep = "\n\t"))

	# Work out replicates to compile
	nRep <- if (is.null(replicates)) length(repfiles) else length(replicates)
	.log("Compiling $nRep $dataType files")
	if (!length(replicates)) replicates <- getReplicates(dataType, prefix, "CSV", workingPath = workingPath)

	# Read all text from datasets
	allPaths <- sapply(replicates, .dataGetFullPath, dataType = dataType, workingPath = workingPath, method = "CSV")
	getText <- lapply(1:length(allPaths), function(i, paths, colName ) {
				getData <- readLines(paths[i])
				paste(c(colName, rep(i, length(getData)-1)), getData, sep=",")
			}, paths = allPaths, colName = paste("\"", replicateCol, "\"", sep=""))

	# Check columns
	firstRows <- sapply(getText, "[[", 1)
	if (!all(firstRows == firstRows[1])) ectdStop("All analysis outputs must have same column names")

	# Write to external file
	outText <- c(getText[[1]][1], do.call("c", lapply(getText, "[", -1)))
	summaryFile <- file.path(workingPath, sprintf("%sSummary.csv", prefix ))
	cat(outText, file = summaryFile, sep="\n")
}
