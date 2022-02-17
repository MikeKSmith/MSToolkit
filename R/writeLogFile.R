#' Write a job log file
#' 
#' Writes a log file of the status of all jobs that are currently executing on
#' the grid.  If all jobs are done, the log will indicate which ran
#' successfully and which did not.  If not, the log file will show the amount
#' of time all jobs have been running, along with the number of jobs of each
#' status.
#' 
#' 
#' @param jobStatuses (Required) A vector of strings of statuses of all
#' currently running jobs
#' @param startingTime (Required) A time at which the jobs started running
#' @param logFileName (Optional) Name of the file to write to ("jobstatus.log"
#' by default)
#' @param statusNames (Optional) A vector of strings that contain all possible
#' names of statuses that jobs could be in.  By default, this is "PEND", "RUN",
#' "DONE", "EXIT", "SUSPEND" and "UNKWN"
#' @param workingPath (Optional) The working directory in which to create the
#' log file.  By default, the current working directory is used
#' @return None.
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @keywords IO
#' @examples
#' 
#'  \dontrun{ 
#'     jstatuses <- sample(c("PEND", "RUN", "DONE", "EXIT"), 10, replace = TRUE)
#'     writeLogFile(jstatuses, Sys.time())
#'     file.show( "jobstatus.log" )
#'   }
#' 
writeLogFile <- function(
  jobStatuses,           # A vector that indicates the status of the jobs that are currently running
  startingTime,        # The starting time of the jobs
  logFileName = "jobstatus.log",
  statusNames = c("PEND", "RUN", "DONE", "EXIT", "SUSPEND", "UNKWN"),
  workingPath = getwd()
) 
{
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # writeLogFile.R, July 13
  #
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION: Writes a log file of jobs currently running
  # KEYWORDS: Documented in Pfizer ECTD Project Support Functions Design Specification
  ###############################################################################

  # Create path to log file
  fullLogFile <- file.path(workingPath, logFileName)
  
  # Perform basic input checks
  if(length(jobStatuses) == 0) ectdStop("Input vector is empty")
  
  # Check for a single starting time
  if (length(startingTime) != 1) ectdStop("Must supply a single starting time")
  
  # Group suspend status to "SUSPEND" item
  suspendTest <- jobStatuses %in% c("PSUSP", "USUSP", "SSUSP", "ZOMBI")
  if (any(suspendTest)) jobStatuses[suspendTest] <- "SUSPEND"
  
  # Group "wait" status into "pending"
  waitTest <- jobStatuses == "WAIT" 
  if (any(waitTest)) jobStatuses[waitTest] <- "PEND"
  
  # Check values passed into job status vector
  if (!all(jobStatuses %in% statusNames)) { 
    missingStatuses <- paste(jobStatuses[jobStatuses%!in%statusNames], collapse=", ")
    ectdStop(paste("Input vector contains unknown status values", missingStatuses, sep=" "))
  }

  # Check to see if all jobs are done.
  if(all(jobStatuses %in% c("DONE", "EXIT")))  {
    nDone <- sum(jobStatuses == "DONE")
    nFail <- sum(jobStatuses == "EXIT")
    cat("\nAll jobs completed\n\nSuccessful runs:", nDone, "\nUnsuccessful runs:", nFail, "\n", file=fullLogFile)
    return(invisible())
  }    
  
  # Not all jobs done, print log                                                            
  cat("\nSome grid jobs not yet completed", file=fullLogFile)
  diffTime <- Sys.time() - startingTime
  timeString <- paste("\nTime since the start of execution:", diffTime, attr(diffTime, "units"), "\n")
  cat(timeString, file = fullLogFile, append = TRUE)

  statusTable <- table(jobStatuses)
  correctOrder <- order(match(names(statusTable), statusNames))
  statusTable <- statusTable[correctOrder]
  
  # Compute the percentages for each status and format them 
  statusText <- sprintf("%15s : %5d (%3d%%)", names(statusTable), statusTable, round(100 * statusTable / sum(statusTable) ))

  # Write out the file
  cat(statusText, file=fullLogFile, append=TRUE, sep = "\n")

  invisible()
}
