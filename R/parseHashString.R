#' Process a string seperated by hash symbols or a vector (or list) of strings
#' not containing any hash symbols
#'
#' \code{parseHashString} constructs a list of numerical vectors from a single
#' string that stores these vectors as comma separated substrings delimited by
#' a hash symbols.  Alternatively if it is passed a list or vector of strings
#' each of which is a comma-separated list of numbers, it will produce a list
#' of numerical vectors by processing these strings with \code{parseCharInput}
#'
#' parseHashString does one of four things depending on the arguments passed to
#' it: (1) If the input is a single string composed of commas, numbers and hash
#' symbols, it will split the string into along the hash symbols and then
#' process the resulting strings with \code{parseCharInput}.  The result will
#' be a list of numerical vectors.  For example,
#' \code{parseHashString("1,2\#3,4")} will create a list with elements c(1,2)
#' and c(3,4).
#'
#' (2) If the input is a vector of strings that don't have any hash symbols, it
#' will it will simply apply parseCharInput to each element of the vector and
#' construct a list from the result.  Thus the same list created in the
#' previous example could also be created by calling
#' \code{parseHashString(c("1,2", "3,4")}.
#'
#' (3) If the input is a list of character strings, it functions essentially
#' the same as in (2).
#'
#' (4) If the input is a list of numerical vectors, it returns the input
#' without modification.
#'
#' @param input (Required) A string to process
#' @param \dots (Optional) Additional arguments to pass to parseCharInput.  No
#' additional arguments passed by default
#' @param missingMsg (Optional) Message to display if the input is not
#' provided.  A standard message is displayed by default
#' @return A list of numerical vectors.
#' @author Romain Francois
#' @seealso \code{\link{parseCharInput}}
#' @keywords misc
#' @examples
#'
#' parseHashString("1.5, 2, 3.2#5, 4.2#10,11")
#' parseHashString(c("1.5, 2, 3.2","5, 4.2","10,11"))
#' parseHashString(list("1.5, 2, 3.2","5, 4.2","10,11"))
#'
#'
parseHashString <- function(input,
                            ...,
                            missingMsg) {
  ##############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # parseHashString.R Thu Jun 07 10:03:11 BST 2007 @418 /Internet Time/
  #
  # Author: Romain Francois
  ##############################################################################
  # DESCRIPTION: parses a hash and comma separated string.
  # KEYWORDS: component:support
  ##############################################################################

  .requiredArgs(input, missingMsg)

  ## check if there is anything to do
  if (is.null(input))
    return(input)
  if (!is.list(input) && any(regexpr("\\#", input)  > 0)) {
    input <- as.list(unlist(strsplit(input, "\\#+")))
  }
  out <-  lapply( input, parseCharInput, ... )

  out
}

