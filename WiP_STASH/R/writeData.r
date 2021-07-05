#' Write scenario data
#'
#' Writes a single element of scenario data (replicate, micro evaluation or
#' macro evaluation) to a file.
#'
#'
#' @param dat (Required) A data frame containing the data to be written
#' @param dataNumber (Required) The entry number of the data - this must be a
#' whole number between 1 and 9999
#' @param dataType (Optional) The type of data to be written - this must be
#' either "MacroEvaluation" "MicroEvaluation" or "ReplicateData" (default is
#' "ReplicateData")
#' @param workingPath (Optional) The path of the current scenario.  The working
#' directory is used by default
#' @param append (Optional) A logical value.  If the file for the entry of data
#' you are specifying already exists, setting append to TRUE will cause
#' writeData to append to the end of that file.  Otherwise it will overwrite
#' the file.  The default is FALSE
#' @param prefix (Optional) Prefix for exported file name
#' @param method (Optional) Data storage method (i.e. where to extract the data
#' from).  Given by \link{getEctdDataMethod} by default
#' @return Returns TRUE if the file was successfully created
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso \code{\link{readData}}
#' @keywords IO
#' @examples
#'
#'   \dontrun{
#'     createDirectories()
#'     x <- matrix(data =
#'     c(0,0,0,0,0,4,4,4,4,4,0,10,25,50,100,0.76,16.91,33.12,49.39,65.73,0.21,1.3,2.44,2.33,4.55,
#'     -6.96,9.19,25.4,41.67,58,8.48,24.63,40.84,57.12,73.45,50,50,50,50,50,0,0,0,0,0,0,0,0,0,0),
#'     nrow = 5, ncol = 10)
#'     x <- as.data.frame(x)
#'     colnames(x) <- c("INTERIM","INTERIMC","D","MEAN","SE","LOWER","UPPER","N","DROPPED",
#'     "STOPPED")
#'     writeData(dat = x, dataNumber = 199, dataType = "Micro")
#'
#'     x <- as.data.frame(matrix(c(0,1),ncol=2,nrow=1))
#'     colnames(x) <- c("Dropped", "Stopped")
#'     writeData(dat = x, dataNumber = 5, dataType = "Macro")
#'   }
#'
"writeData" <- function(dat,
                        dataNumber,
                        dataType = c("ReplicateData", "MacroEvaluation", "MicroEvaluation"),
                        workingPath = getwd(),
                        append = FALSE,
                        prefix = switch(
                          dataType,
                          ReplicateData = "replicate",
                          MicroEvaluation = "micro",
                          MacroEvaluation = "macro"
                        ),
                        method = getEctdDataMethod()   #@ Data Storage Method to use)
                        {
                          ###############################################################################
                          # Mango Solutions, Chippenham SN14 0SQ 2006
                          # writeData.R Wed Jun 20 20:23:26 BST 2007 @418
                          #
                          # Author: Francisco Gochez
                          ###############################################################################
                          # DESCRIPTION: Writes a single entry of data to a file.  Returns TRUE if the operation was successful and FALSE otherwise.
                          # KEYWORDS: IO
                          ###############################################################################

                          dataType <-
                            match.arg(dataType)                                    # Check data type
                          if (!is.data.frame(dat))
                            ectdStop("dat is not a data frame")        # Check that dat is a data frame
                          if (dataType != "ReplicateData")
                            method <- "CSV"

                          switch(
                            method,
                            "CSV" = {
                              # Get full path to data
                              fullPath <-
                                .dataGetFullPath(
                                  dataNumber = dataNumber,
                                  dataType = dataType,
                                  workingPath = workingPath,
                                  method = method,
                                  prefix = prefix
                                )

                              # Write the file
                              tryWrite <-
                                try(write.table(
                                  dat,
                                  fullPath,
                                  append = append,
                                  row.names = FALSE,
                                  sep = ",",
                                  quote = F
                                ))
                              if (class(tryWrite) == "try-error")
                                ectdStop("Error when writing the data to file $fullPath:\n\t$tryWrite")
                            },
                            "RData" = {
                              # Get full path to data
                              fullPath <-
                                .dataGetFullPath(
                                  dataNumber = dataNumber,
                                  dataType = dataType,
                                  workingPath = workingPath,
                                  method = method,
                                  prefix = prefix
                                )

                              # Write the file
                              tryWrite <- try(.writeToRData(dat, fullPath, append = append))
                              if (class(tryWrite) == "try-error")
                                ectdStop("Error when writing the data to file $fullPath:\n\t$tryWrite")
                            },
                            "Internal" = {
                              .ectdEnv$DataStore[[dataNumber]] <- dat
                            },
                            ectdStop("Data storage method not recognised")
                          )
                          .log("writing $dataType data number $dataNumber")
                          invisible(TRUE)
                        }
