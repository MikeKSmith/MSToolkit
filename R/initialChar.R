#' Convert the first letter that occurs in a string to lower case
#'
#' By default, initialChar finds the first letter that occurs in a string and
#' returns its lower case version.  The user may use the "adm" argument to
#' restrict the admissable letters which may be returned (by default this will
#' be all possible lower case letters).
#'
#'
#' @param txt (Required) The string input to process
#' @param adm (Optional) Regular expression which may be used to restrict the
#' set of letters which will be returned.  By default it will allow all lower
#' case letters
#' @param err (Optional) The error message which will be printed if no
#' admissable letters (as determined by adm) are found.  By default, message
#' "Not acceptable value" will be displayed
#' @return initalChar returns a lower case version of the first letter
#' contained in the string "txt" if that character is admissable.  If it is not
#' (or no letter is found), an error message will be printed.
#' @note This function is not case sensitive.
#' @author Romain Francois
#' @keywords misc
#' @examples
#'
#' initialChar("100 Kilometers")
#' \dontrun{
#'    # The first character is "A", but "a" is not in the list of admissable letters
#'     initialChar("Allen, Alistair, Atticus", adm = "irt" )
#'   }
#'
initialChar <- function(
  txt,                                    # text to parse
  adm = "[:lower:]",                      # acceptable values for the output
  err = "Not acceptable value"            # error message if not in the acceptable values
  ){
 	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# initialChar.R Fri Sun Jun 03 20:05:15 BST 2007 @836 /Internet Time/
	#
	# Author: Romain Francois
	###############################################################################
	# DESCRIPTION: gets the first letter from a character string and "lower case" it
  # KEYWORDS: check, component:support
	###############################################################################
  
  ### tidy up the input
  if(missing(txt) || is.null(txt) || length(txt) == 0) 
    ectdStop("no character string to use")
  if(length(txt) > 1) ectdWarning("only the first element has been used")
  txt <- as.character( txt[1] )  
  
  ### figure out where is the first letter in that string and extract it
  re <- regexpr("[[:alpha:]]", txt)
  if( re == -1 ) ectdStop("No character in the string")
  out <- casefold( substring(txt,re,re ) )
  
  ### checks if the output is one of the possibilities given
  rx <- paste("[", casefold(adm) , "]", sep = "")
  if( out %!~% rx ) ectdStop( err )
  
  out
}

