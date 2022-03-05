#' Check if a character string is a valid R variable name
#'
#' Checks if a character string is a valid R variable name using regular
#' expressions.
#'
#' A valid R name starts with a letter or a dot followed by a non-numerical
#' character and contains only letters, numbers and dots.
#'
#' @param \dots (Required) A collection of character vectors to be checked
#' @return None. The function is simply generates an error if an invalid name
#' is passed to it.
#' @author Romain Francois
#' @keywords error
#' @examples
#'
#'   # correct name
#'   validNames("DOSE")
#'   \dontrun{
#'     # wrong name
#'     validNames(".334fsedqw")
#'   }
#'
#' @export
validNames <- function(...) {
  ##############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # validNames.R Fri Jun 01 14:06:41 BST 2007 @587 /Internet Time/
  #
  # Author: Romain Francois
  ##############################################################################
  # DESCRIPTION: check if the names are valid R names
  # KEYWORDS: component:support check
  ##############################################################################

  sapply(list(...), function(x) {
    if (!is.null(x) && x %!~% "^[\\.]?[a-zA-Z][\\.0-9a-zA-Z]*$") {
      ..nm <- get("..nm")
      wrongs <- paste(x[..nm], collapse = ", ")
      ectdStop("$wrongs : invalid R name(s)")
    }
  })
  TRUE

}

.checkLogical <- function(..., single = TRUE) {
  inList <- list(...)
  isOk <- sapply(inList, is.logical)
  if (single)
    isOk <- isOk & sapply(inList, length) == 1
  if (any(myTest <- !isOk)) {
    theCall <- match.call(expand.dots = TRUE)
    theCall <- as.character(theCall)[1 + 1:length(inList)]
    whichNot <- theCall[myTest]
    errorMessage <-
      paste(if (single)
        "Single character"
        else
          "Character", "expected:")
    ectdStop(paste(errorMessage, paste(whichNot, sep = ", ")))
  }
  isOk
}

.checkNumeric <- function(..., single = TRUE) {
  inList <- list(...)
  isOk <- sapply(inList, is.numeric)
  if (single)
    isOk <- isOk & sapply(inList, length) == 1
  if (any(myTest <- !isOk)) {
    theCall <- match.call(expand.dots = TRUE)
    theCall <- as.character(theCall)[1 + 1:length(inList)]
    whichNot <- theCall[myTest]
    errorMessage <-
      paste(if (single)
        "Single character"
        else
          "Character", "expected:")
    ectdStop(paste(errorMessage, paste(whichNot, sep = ", ")))
  }
  isOk
}

.checkCharacter <- function(..., single = TRUE) {
  inList <- list(...)
  isOk <- sapply(inList, is.character)
  if (single)
    isOk <- isOk & sapply(inList, length) == 1
  if (any(myTest <- !isOk)) {
    theCall <- match.call(expand.dots = TRUE)
    theCall <- as.character(theCall)[1 + 1:length(inList)]
    whichNot <- theCall[myTest]
    errorMessage <-
      paste(if (single)
        "Single character"
        else
          "Character", "expected:")
    ectdStop(paste(errorMessage, paste(whichNot, sep = ", ")))
  }
  isOk
}
