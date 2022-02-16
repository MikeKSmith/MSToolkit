#' Convert R to SAS Statements
#'
#' Converts R statements to SAS statements, to be included in a batch call to
#' the SAS system
#'
#' The function will accept either a character vector or a character matrix
#' with 3 columns.
#'
#' If the input is a vector, the function assumes these are logical statements
#' to be converted to SAS code.  The converted statements are prefixed with an
#' "IF" statement and collapsed based on semi- colons.
#'
#' If the input is a 3 column vector, the function will convert the first
#' column of (logical) statments to SAS code, and prefix them with an "IF"
#' statement.  The 2nd column is pasted together with the first column,
#' seperated by a "THEN" statement.  The 3rd column is pasted together with the
#' third column seperated by an "=" sign.  The whole string is collapsed based
#' on ";" symbols and returned.
#'
#' @param code (Required) R code elements to be converted to SAS statements
#' @return A single character string representing the "SAS" version of the
#' input code
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @keywords IO
#' @examples
#'
#' \dontrun{
#'
#' convertToSASCode(c("X > 1", "X < 0", "Y > 0 & Y < 10"))
#' #  [1] "IF X > 1 ;IF X < 0 ;IF Y > 0 AND Y < 10 ;"
#'
#' sasMat <- cbind(c("X > 1", "X < 0", "Y > 0 & Y < 10"), c("Col1", "Col2", "Col3"), 1:3)
#' sasMat
#' #     [,1]             [,2]   [,3]
#' #[1,] "X > 1"          "Col1" "1"
#' #[2,] "X < 0"          "Col2" "2"
#' #[3,] "Y > 0 & Y < 10" "Col3" "3"
#' convertToSASCode(sasMat)   # Convert matrix of "data change" code
#' # [1] "IF X > 1 THEN Col1 = 1 ;IF X < 0 THEN Col2 = 2 ;IF Y > 0 AND Y < 10 THEN Col3 = 3 ;"
#'
#' }
convertToSASCode <- function(
  code    #@ matrix of 3 columns describing the changes to make to the data
){
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # convertToSASCode.R Wed Jun 27 12:46:29 BST 2007 @532 /Internet Time/
  #
  # Author: Romain, Rich
  ###############################################################################
  # DESCRIPTION: convert dataChanges to SAS code
  # KEYWORDS: component:analysis
  ###############################################################################
  # TESTME

  # Parse inputs
  if (!length(code)) return("")
  if (!is.matrix(code) & !is.vector(code)) ectdStop("Unrecognised format for SAS Code conversion")
  if (is.matrix(code) && ncol(code) != 3) ectdStop("Matrix to convert to SAS Code must have 3 columns")
  # <TODO>
  # <=, >=, <, >
  repSas <- cbind(c("==", "!=", "\\&", "\\|"), c("EQ", "NE", "AND", "OR"))  # Replace strings for SAS syntax
  # </TODO>

  # Data changes if a matrix has been provided
  sasCode <- if (is.matrix(code)) {
     for (i in 1:nrow(repSas)) code[,1] <- gsub(repSas[i,1], repSas[i,2], code[,1])
     paste("IF", code[,1], "THEN", code[,2], "=", code[,3], ";", collapse="")
  }
  else {
     for (i in 1:nrow(repSas)) code <- gsub(repSas[i,1], repSas[i,2], code)
     paste("IF", code, ";", collapse="")
  }
  sasCode
}
