#' Parse a comma seperated string
#'
#' The "parseCharInput" function converts a comma-seperated string into a
#' vector by splitting the string along the commas.  If "convertToNumeric" is
#' true, the elements of the string will be converted into numbers and a
#' numeric vector will be returned.
#'
#'
#' @param input (Required) The string to be processed
#' @param convertToNumeric (Optional) A logical value.  Should the input should
#' be converted into numerics?  TRUE by default
#' @param sort (Optional) A logical value.  Should the resulting vector be
#' sorted.  FALSE by default
#' @param expected (Optional) The expected length of the output vector.  No
#' check is performed by default
#' @param msg (Optional) Error message to print if the input is not of the
#' correct length specified by "expected".  Default is "Wrong length"
#' @param checkdup (Optional) A logical value. Should an error be generated if
#' there are any duplicated value?  FALSE by default
#' @param missingMsg (Optional) Error message to print if the input is missing.
#' Default is "Unknown"
#' @param checkProb (Optional) A logical value.  Should the function check that
#' the output vector sums to one?  FALSE by default
#' @param valid (Optional) A logical value. Should the \code{\link{validNames}}
#' function be used to check the validity of the extracted names?  FALSE by
#' default
#' @return A numeric vector or a character vector depending on the
#' \code{convertToNumeric} flag.
#' @author Romain Francois
#' @seealso \code{\link{parseHashString}} uses \code{parseCharInput}
#' sequentially.
#' @keywords misc
#' @examples
#'
#'   parseCharInput( "1,2,4")
#'   parseCharInput( "1,6,4", sort = TRUE )
#'
#'   \dontrun{
#'   ## see also the unit tests for more examples
#'   file.show( system.file( "Runit", "runit.supportfunctions.R" ,
#'     package = "MSToolkit"))
#'   }
#'@export
parseCharInput <- function(input,
                           convertToNumeric = TRUE,
                           sort = FALSE,
                           expected,
                           msg = "wrong length",
                           checkdup = FALSE,
                           missingMsg = 'Unknown',
                           checkProb = FALSE,
                           valid = FALSE) {
  ##############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # parseCharInput.R Fri Wed Jun 06 12:55:37 BST 2007 @538 /Internet Time/
  #
  # Author: Romain Francois
  ##############################################################################
  # DESCRIPTION: parses a csv character string
  # KEYWORDS: check, component:support
  ##############################################################################

  missing(input) && ectdStop(missingMsg)

  if (is.null(input))
    return(NULL)

  out <- if (is.numeric(input))
    input
  else {
    ## handles the commas (TESTFIX)
    if( any( regexpr(",$", input)  > 0 ) )
      ectdStop("Traling comma not accepted")

    if( any( regexpr("^,", input)  > 0 ) )
      ectdStop("Leading comma not accepted")
    if( any( regexpr(",{2,}", input)  > 0 ) )
      ectdStop("The separator should only be **one** comma")

    ## a numeric value is made numbers, dots, plus, minus and possibly e for the
    ## scientific notation (is there a need to be more clever with the
    ## scientific notation)
    if (convertToNumeric &&
        any(regexpr("[^0-9eE\\.,[:space:]\\+\\-]", input)  > 0))

      ectdStop("Impossible to convert to numbers")
    out <- unlist(strsplit(input, "[[:space:]]*,[[:space:]]*"))
    if (convertToNumeric)
      out <- as.numeric(out)
    out
  }

  valid && validNames( out )

  if( !missing(expected) && length(out) != expected )
    ectdStop( msg )
  if( checkdup && any(duplicated(out)))
    ectdStop( "Duplicated values in " %.% deparse(substitute(input)))

  ## convert to numeric and sort if required
  if(checkProb  ){
    #if(!is.numeric(out) || sum(out) != 1 )
    if(!is.numeric(out) || !all.equal(sum(out), 1) )
      ectdStop("Parsed values don't sum up to one")
  }
  if(sort) {
    sort(out)
  } else {
    out
  }
}

