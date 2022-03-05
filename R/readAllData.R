#' Read scenario data
#'
#' Reads a single element of replicate, macro evaluation or micro evaluation
#' data from the current scenario
#'
#'
#' @param dataType (Optional) The type of data you wish to read - either
#' "ReplicateData", "MacroEvaluation" or "MicroEvaluation".  Default is
#' "ReplicateData"
#' @param workingPath (Optional) The working path of the current scenario.  The
#' current working directory is used by default.
#' @param dirName (Optional) Subdirectory of workingPath from which to read
#' data.  By default, this is taken as the same as the "dataType" input
#' @param prefix (Optional) Prefix of file name to use (eg. "micro" for Micro
#' Evaluation files).  Default is "Replicate" when dataType is "ReplicateData",
#' "Micro" when dataType is "MicroEvaluation" or "Macro" when dataType is
#' "MacroEvaluation"
#' @param replicates (Optional) A vector identifying a subset of replicates to
#' import from the directory.  By default, all replicates are imported
#' @param consolidate (Optional) Should the function combine the seperate data
#' frames into a single data frame.  TRUE by default
#' @param replicateCol (Optional) Name for created "Replicate" variable in the
#' consolidated data frame (default given by \link{getEctdColName})
#' @param method (Optional) Data storage method (i.e. where to extract the data
#' from).  Given by \link{getEctdDataMethod} by default
#' @return If consolidate is FALSE, the function will return a list of data
#' frames.  If consolidate is TRUE, the function will combine this list into a
#' single data frame for return
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso \code{\link{writeData}}, \code{\link{readData}}
#' @keywords IO
#' @examples
#'
#' 	\dontrun{
#'    readAllData(dataType = "Macro", consolidate = FALSE)
#' 	 readAllData(dataType = "Micro")
#'  }
#'
#' @export
"readAllData" <- function(
  dataType = c("ReplicateData", "MicroEvaluation", "MacroEvaluation"),    #@ Type of data, should be "Replicate", "Macro", or "Micro"
  workingPath = getwd(),      #   The working directory
  dirName = dataType,       #@ Directory to use in case one doesn't want to use the default directory
  prefix = switch(dataType, ReplicateData = "replicate", MicroEvaluation = "micro", MacroEvaluation = "macro"),
  replicates = NULL,
  consolidate = TRUE,
  replicateCol = getEctdColName("Replicate"),
  method = getEctdDataMethod()   #@ Data Storage Method to use
){
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # Thurs Jun 21 16:51 BST 2007 @445 /Internet Time/
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION: Returns all data for a particular data type, or a subset of
  # the data if replicates is specified.  Returns either a list of data frames or
  # if consolidate is set to TRUE, consolidates into a single data frame
  # KEYWORDS: IO
  ###############################################################################

  # Check dataType input
  dataType <- match.arg(dataType)
  if (dataType != "ReplicateData") method <- "CSV"

  # Attempt to extract all replicate numbers
  if (!length(replicates)) replicates <- getReplicates(dirName, prefix, method, workingPath = workingPath)

  # Read all replicate data using call to "readData"
  out <- lapply(replicates, readData, dataType=dataType, workingPath = workingPath, method = method)
  names(out) <- sprintf("%04d", replicates)

  # Consolidate datasets if required
  if (consolidate) {
    repColData <- rep(replicates, sapply(out, nrow))
    columnNames <- unique(unlist(lapply(out, names)))
    out <- lapply( out, function(d) {
      missings <- columnNames [ !(columnNames %in% names(d))]
      if (length(missings) > 0 ) d[,missings] <- NA
      d
    })
    out <- do.call( rbind, out )
    out[[replicateCol]] <- repColData
    out <- out[ , c(replicateCol, setdiff(names(out), replicateCol))]
    rownames(out) <- as.character(1:nrow(out))
  }

  out
}
