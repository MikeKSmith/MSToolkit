#' Check that column names are found in a data frame
#'
#' Checks that all column names specified in "cols" are found in the data frame
#' names (given by "dNames") and produces a neat error message if not
#'
#' @param dNames Character vector of column names from a data frame
#' @param cols Character vector of column names that should be found in the
#' data frame
#' @return A TRUE value (if all of "cols" are found in "dNames") or an
#' exception if not
#' @author Romain Francois
#' @examples
#'
#' 	checkColNames(LETTERS, "D")
#' 	checkColNames(LETTERS, LETTERS[1:5])
#' 	\dontrun{
#' 	  # Will show an error because "Hello" is not in LETTERS
#' 	  checkColNames(LETTERS, c("A", "Hello"))
#' 	}
#'
checkColNames <- function(dNames, cols) {
  validNames(cols)		# Check column names are valid inputs
  colTest <- cols %in% dNames
  if (!all(colTest)) {
    missCols <- paste("\"", cols[!colTest], "\"", sep = "", collapse = ", ")
    ectdStop(paste("Some required columns missing from the data:", missCols))
  }
  TRUE
}
