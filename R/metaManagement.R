
#######################################################################################################

## Set the Environment - this code executes on build to ensure it exists in the MSToolkit library!

# This line sets the initial (empty) environment
.ectdEnv <- new.env( )

# This line sets the default "logging" file to "ectd.log"
assign( "logfile", "ectd.log", envir = .ectdEnv )

# This line sets the default "verbose" behaviour, which determines the amount of logging performed
# This is used, in particular, when errors are generated
assign( "verbose", FALSE, envir = .ectdEnv )

# This line sets the default data format (used for the format of data in the logging)
assign( "dateFormat","%Y-%m-%d %H:%M:%OS4" , envir = .ectdEnv )

# This sets the current data storage method
assign("dataStoreMethod", "CSV", envir = .ectdEnv)

# This sets the default column names
assign(
  "colNames",
  list(
    Subject = list(
      Name = "SUBJ",
      Other = "ID",
      Default = "SUBJ"
    ),
    Time = list(
      Name = "TIME",
      Other = c("DAY", "WEEK"),
      Default = "TIME"
    ),
    Dose = list(
      Name = "DOSE",
      Other = "",
      Default = "DOSE"
    ),
    Interim = list(
      Name = "INTERIM",
      Other = "",
      Default = "INTERIM"
    ),
    ParOmit = list(
      Name = "PAROMIT",
      Other = "",
      Default = "PAROMIT"
    ),
    RespOmit = list(
      Name = "RESPOMIT",
      Other = "",
      Default = "RESPOMIT"
    ),
    Response = list(
      Name = "RESP",
      Other = "DV",
      Default = "RESP"
    ),
    Trt = list(
      Name = "TRT",
      Other = "",
      Default = "TRT"
    ),
    Missing = list(
      Name = "MISSING",
      Other = "",
      Default = "MISSING"
    ),
    Replicate = list(
      Name = "Replicate",
      Other = "TRIAL",
      Default = "Replicate"
    ),
    DrugName = list(
      Name = "DRUGNAME",
      Other = "",
      Default = "DRUGNAME"
    ),
    Drug = list(
      Name = "DRUG",
      Other = "",
      Default = "DRUG"
    )
  ),
  envir = .ectdEnv
)

#######################################################################################################

## The rest of this script sets the access functions for the meta layer

# get or set the logfile
getEctdLogFile <- function()
  get("logfile", envir = .ectdEnv)

setEctdLogFile <- function(file) {
  if (missing(file))
    ectdStop("Must provide log file")
  assign("logfile", file, envir = .ectdEnv)
  invisible(file)
}

# get or set the verbose
getEctdVerbose <- function()
  get("verbose", envir = .ectdEnv)



#' MSToolkit package options
#'
#' Options used by the MSToolkit package to control the logfile, the amount of
#' messages that are written in the logfile, and the format of the date.
#'
#' The three function write and read information from the (not exported)
#' environment \code{.ectdEnv}.  These settings are mainly used by the (not
#' exported) \code{.log} function.
#'
#' @aliases setEctdVerbose setEctdLogFile setEctdDateFormat getEctdVerbose
#' getEctdLogFile getEctdDateFormat
#' @param verbose (Required) A logical value.  If set to TRUE, messages are
#' sent to the logfile during the process of generating the data and analyzing
#' it. Set to \code{TRUE} when attaching the package.
#' @return The function (invisibly) returns the previous value of the
#' arguments.
#' @seealso \code{\link{options}} provides a similar mechanism for R options.
#' @keywords IO
#' @examples
#' \dontrun{
#'   oldverb <- setEctdVerbose( TRUE )
#'   olddf   <- setEctdDateFormat("%Y")
#'   oldlf   <- setEctdLogFile("mstoolkit.log")
#'
#'   for( i in 1:100 ) {
#'     MSToolkit:::.log( paste("some message:", i) )
#'   }
#'   file.show( getEctdLogFile() )
#'
#'   setEctdVerbose   (oldverb)
#'   setEctdDateFormat(olddf  )
#'   setEctdLogFile   (oldlf  )
#'
#' }
setEctdVerbose <- function(verbose) {
  if (missing(verbose))
    ectdStop("Must provide verbose flag")
  assign("verbose", verbose, envir = .ectdEnv)
  invisible(verbose)
}

# get or set the dateFormat
getEctdDateFormat <- function()
  get("dateFormat", envir = .ectdEnv)

setEctdDateFormat <- function(format) {
  if (missing(format))
    ectdStop("Date format must be provided")
  assign("dateFormat", format, envir = .ectdEnv)
  invisible(format)
}

# Get or set the data storage method
getEctdDataMethod <-
  function()
    get("dataStoreMethod", envir = .ectdEnv)



#' Current data storage method
#'
#' Gets and sets the current "data storage" method, used for the storage of
#' simulated trial data
#'
#' Gets (getEctdDataMethod) and sets (setEctdDataMethod) the current "data
#' storage" method, used for the storage of simulated trial data. The choices
#' of storage method are: * CSV - Replicate data stored in seperate CSV files
#' outside of R * RData - Replicate data stored in seperate RData files outside
#' of R * Internal - Replicate data stored as a list of data frames in an
#' internal environment (.ectdEnv$DataStore)
#'
#' Note: The data storage only impacts replicate data - micro and macro
#' evaluation data is always held as CSV files
#'
#' @aliases setEctdDataMethod getEctdDataMethod
#' @param method (Required) The data storage method to use (either 'CSV',
#' 'RData' or 'Internal'
#' @return The "getEctdDataMethod" function returns the current data method
#' ("CSV", "RData" or "Internal") The "setEctdDataMethod" function invisibly
#' returns the method that has just been set as the default
#' @keywords IO
#' @examples
#' \dontrun{
#'
#' 	nowMethod <- getEctdDataMethod()
#'
#' 	setEctdDataMethod("CSV")
#' 	getEctdDataMethod()
#'
#' 	setEctdDataMethod("RData")
#' 	getEctdDataMethod()
#'
#' 	setEctdDataMethod("Internal")
#' 	getEctdDataMethod()
#'
#' 	setEctdDataMethod(nowMethod)
#'
#' }
setEctdDataMethod <- function(method) {
  if (missing(method))
    ectdStop("Must provide a data storage method: 'CSV', 'RData' or 'Internal'")
  method <- match.arg(method, c("CSV", "RData", "Internal"))
  assign("dataStoreMethod", method, envir = .ectdEnv)
  invisible(method)
}

# Get & Set external execution path
getEctdExternalPath <- function(pathName) {
  getPaths <- get("externalPaths", envir = .ectdEnv)
  if (missing(pathName)) {
    return(names(getPaths))
  }
  else {
    if (!is.character(pathName) ||
        length(pathName) != 1)
      ectdStop("Single character value should be provided as the 'pathName' input")
    if (pathName %in% names(getPaths))
      return(getPaths[pathName])
    else
      ectdStop(paste("Could not find external path '", pathName, "'", sep = ""))
  }
}



#' Controls paths to a set of external execution paths
#'
#' Gets and sets paths to external
#'
#'
#' getEctdExternalPath gets the execution path for a specific application on a
#' particular environment setEctdExternalPath sets an execution path for an
#' application
#'
#' More permanent changes can be made by modifying the "ECTD.ini" file in the
#' library root
#'
#' @aliases setEctdExternalPath getEctdExternalPath
#' @param pathName Name of the path to return or set.  When using
#' getEctdExternalPath, leave pathName blank to return a vector of available
#' path names
#' @param Value New value for the path
#' @return The "getEctdExternalPath" function returns the current execution
#' path for a given "pathName" The "setEctdExternalPath" function invisibly
#' returns the updated path list
#' @keywords IO
#' @examples
#' \dontrun{
#'
#' 	getEctdExternalPath()		# Look at available paths
#'
#' 	getEctdExternalPath("SASPATH_WIN")		# Get the "SAS Execution on Windows" path
#'
#' }
setEctdExternalPath <- function(pathName, Value) {
  if (missing(pathName) ||
      !is.character(pathName) ||
      length(pathName) != 1)
    ectdStop("Single character value should be provided as the 'pathName' input")
  if (missing(Value) ||
      !is.character(Value) ||
      length(Value) != 1)
    ectdStop("Single character value should be provided as the 'Value' input")
  getPaths <- get("externalPaths", envir = .ectdEnv)
  getPaths <- getPaths [setdiff(names(getPaths), pathName)]
  getPaths <- c(getPaths, Value)
  names(getPaths)[length(getPaths)] <- pathName
  assign("externalPaths", getPaths, envir = .ectdEnv)
  invisible(getPaths)
}

# Get, set and reset default column names

#' Control of default column names
#'
#' Functions that allow control over the default column names for simulated
#' data
#'
#'
#' The functions provide the following capabilities:
#'   * getEctdColName - Gets the current default column name given a column
#'   type
#'   * setEctdColName - Sets the current default column name for a column
#'   type
#'   * resetEctdColNames - Resets the current default column names to their
#'    initial state
#'    * getEctdPossibleColNames - Gets the set of possible column names given a
#' column type
#'   * setEctdPossibleColNames - Sets the set of possible column names
#' given a column type
#'   * matchEctdColNames - Selects a column name from a set of
#' names that best matches the possible column type names
#'
#' The set of possible "column types" are:
#'   * Subject - Subject column
#'   * Dose - Dose column
#'   * Time - Time column
#'   * Replicate - Replicate column names
#'   * Interim - Interim allocation column name
#'   * ParOmit - Parameter "omit" flag column name
#'   * RespOmit - Response "omit" flag column name
#'   * Response - Response column name
#'   * Trt - Treatment column name
#'   * Missing - Parameter "omit" flag column name
#'   * DrugName - Name of column containing the "Drug Name" (used in typical
#'   value simulations)
#'   * Drug - Name of column containing the "Drug Value" (used in typical
#'   value simulations)
#'
#' @param colName (Required) The "column type" of the variable name of interest
#' (one of 'Subject', 'Time', 'Dose', 'Interim', 'ParOmit', 'RespOmit',
#' 'Response', 'Trt', 'Missing', 'Replicate', 'DrugName' and 'Drug')
#' @return
#'
#' The "getEctdColName" function returns a single character, giving the current
#' column name
#'
#' The "getEctdPossibleColNames" function returns a character vector, giving a
#' set of possible columns
#'
#' The "matchEctdColNames" function returns a single character identifying the
#' variable in "dataNames" that should be used as the "colName" column
#'
#' The other functions to not explicitly return anything
#' @keywords IO
#' @examples
#' \dontrun{
#' 	getEctdColName("Subject")
#' 	setEctdColName("Subject", "ID")
#' 	getEctdColName("Subject")
#' 	resetEctdColNames()
#' 	getEctdPossibleColNames("Subject")
#' 	matchEctdColNames ("Subject", c("A", "SUBJ", "B"))
#' }
getEctdColName <- function(colName) {
  if (missing(colName) ||
      !is.character(colName) ||
      length(colName) != 1)
    ectdStop("Single character value should be provided as the 'colName' input")
  getNames <- get("colNames", envir = .ectdEnv)
  if (colName %in% names(getNames))
    return(getNames[[colName]]$Name)
  else
    ectdStop(paste("Provided column name '",
                   colName, "' cannot be found",
                   sep = ""))
}


#' @describeIn getEctdColName
#'
#' @param colName (Required) The "column type" of the variable name of interest
#' (one of 'Subject', 'Time', 'Dose', 'Interim', 'ParOmit', 'RespOmit',
#' 'Response', 'Trt', 'Missing', 'Replicate', 'DrugName' and 'Drug')
#' @param Value (Required) Value to which to set the default column name
setEctdColName <- function(colName, Value) {
  if (missing(colName) ||
      !is.character(colName) ||
      length(colName) != 1)
    ectdStop("Single character value should be provided as the 'colName' input")
  if (missing(Value) ||
      !is.character(Value) ||
      length(Value) != 1)
    ectdStop("Single character value should be provided as the 'Value' input")
  getNames <- get("colNames", envir = .ectdEnv)
  if (colName %in% names(getNames))
    getNames[[colName]]$Name <- Value
  else
    ectdStop(paste(
      "Provided column name '",
      colName,
      "' not a valid column to set",
      sep = ""
    ))
  assign("colNames", getNames, envir = .ectdEnv)
  invisible(getNames)
}

#' @describeIn getEctdColName
#'
#' @param whichNames Column types for which to reset the default name (default
#' all)
resetEctdColNames <- function(whichNames = names(getNames)) {
  getNames <- get("colNames", envir = .ectdEnv)
  if (!is.character(whichNames))
    ectdStop("Character vector should be provided as the 'whichNames' input")
  whichNames <- whichNames [whichNames %in% names(getNames)]
  if (length(whichNames)) {
    for (i in whichNames)
      getNames[[i]]$Name <- getNames[[i]]$Default
  }
  assign("colNames", getNames, envir = .ectdEnv)
  invisible(getNames)
}

#' @describeIn getEctdColName
#'
#' @param colName (Required) The "column type" of the variable name of interest
#' (one of 'Subject', 'Time', 'Dose', 'Interim', 'ParOmit', 'RespOmit',
#' 'Response', 'Trt', 'Missing', 'Replicate', 'DrugName' and 'Drug')
getEctdPossibleColNames <- function(colName) {
  if (missing(colName) ||
      !is.character(colName) ||
      length(colName) != 1)
    ectdStop("Single character value should be provided as the 'colName' input")
  getNames <- get("colNames", envir = .ectdEnv)
  if (colName %in% names(getNames)) {
    outVec <-
      unique(c(getNames[[colName]]$Name, getNames[[colName]]$Other))
    return(setdiff(unique(outVec), ""))
  }
  else
    ectdStop(paste("Provided column '",
                   colName, "' cannot be found",
                   sep = ""))
}

#' @describeIn getEctdColName
#'
#' @param colName (Required) The "column type" of the variable name of interest
#' (one of 'Subject', 'Time', 'Dose', 'Interim', 'ParOmit', 'RespOmit',
#' 'Response', 'Trt', 'Missing', 'Replicate', 'DrugName' and 'Drug')
#' @param Value (Required) Value to which to set the default column name
setEctdPossibleColNames <- function(colName, Value) {
  if (missing(colName) ||
      !is.character(colName) ||
      length(colName) != 1)
    ectdStop("Single character value should be provided as the 'colName' input")
  if (missing(Value) ||
      !is.character(Value) ||
      !length(Value))
    ectdStop("A character vector of possible names must be provided")
  getNames <- get("colNames", envir = .ectdEnv)
  if (colName %in% names(getNames)) {
    getNames[[colName]]$Other <- unique(Value)
    assign("colNames", getNames, envir = .ectdEnv)
    invisible(getNames)
  }
  else
    ectdStop(paste("Provided column '",
                   colName, "' cannot be found",
                   sep = ""))
}

#' @describeIn getEctdColName
#'
#' @param colName (Required) The "column type" of the variable name of interest
#' (one of 'Subject', 'Time', 'Dose', 'Interim', 'ParOmit', 'RespOmit',
#' 'Response', 'Trt', 'Missing', 'Replicate', 'DrugName' and 'Drug')
#' @param dataNames (Required) Column names against which to match the possible
#' set of column names
matchEctdColNames <- function(colName, dataNames) {
  colList <- getEctdPossibleColNames(colName)
  if (!any(myTest <- colList %in% dataNames))
    return(NULL)
  else
    colList[myTest][1]
}

