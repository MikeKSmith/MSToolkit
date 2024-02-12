#' Parse a numerical range expression
#'
#' parseRangeCode: Converts inequalities of up to two comparators stored in a
#' vector of strings into an executable R expression.  For instance,
#' parseRangeCode(c("1 < Y < 20", "40 < Y")) will yield the expression
#' (1<Y)&(Y<20)&(40<Y).  parseRCode: simply parse an R string into an
#' expression.
#'
#' The \code{parseRangeCode} function converts various kinds of
#' ranges/inequalities that are normally not handled by R into R-executable
#' expressions.  For instance it will convert inequalities of the form "A < B <
#' C" into (A < B) & (B < C), where < may be replaced with any comparator.
#' Moreover it allows inqualities to be concatenated by the symbols "&", ";" or
#' "," which are all treated as equivalent to the logical "and" (i.e. "&").
#' Thus "A < B <C & D > E, F <= G" will be converted into (A < B)&(B < C)&(D >
#' E)&(F <= G). In addition if \code{code} is a vector with more than one
#' element, each element will be parsed and then concatenated into a single
#' expression with "&".  Hence c("1 < Y", "2 < Z <= 5") would become \code{(1 <
#' Y)&(2 < Z)&(Z <= 5)}.
#'
#' @param code (Required) For parseRangeCode: A vector of strings that contain
#' inequalities along with various seperators (see below).  Each inequality
#' should have either 1 or 2 comparators. For parseRCode: a code string. For
#' \code{parseRCode} a character vector containing code.
#' @return An expression constructed as detailed above.
#' @section Warning: The character "|" is not allowed in any of the expressions
#' contained in "code" for the \code{parseRangeCode} function.
#' @author Romain Francois
#' @seealso \code{\link{subset}}, \code{\link{parse}}
#' @keywords misc
#' @examples
#'
#' # Examples of using subsets
#' exData <- data.frame( Y = rnorm(200), B = rnorm( 200 ) )
#' subs1 <- MSToolkit:::parseRangeCode("1 < Y < 10 & 1 > B > -2")
#' exData[ eval(subs1, exData), ]
#'
#' subs2 <- MSToolkit:::parseRangeCode(c("1 < Y < 10", "1 > B > -2"))
#' exData[ eval(subs1, exData), ]
#'
#' expr <- MSToolkit:::parseRCode("rnorm(30)")
#' eval( expr ) 

parseRangeCode <- function(
  code      #@ code to parse
){
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # parseRangeCode.R Thu Jun 21 11:52:00 BST 2007 @494 /Internet Time/
  #
  # Author: Romain
  ###############################################################################
  # DESCRIPTION: parse R code that represents range
  # KEYWORDS: component:support
  ###############################################################################
  if(missing(code) || is.null(code) ) return(NULL)

  ## handle the if
  if( any( regexpr("\\|", code)  > 0 )  )
    ectdStop( "`|` not allowed in the range code" )

  code <- unlist( strsplit( code, "[;&,]" ) )

  ## handle multiple <>
  out <- NULL
  gx  <- gregexpr( "[<>]=?", code  )
  for( i in seq(along = code)){
    gxi <- gx[[i]]
    if(length(gxi) == 1 && gxi == -1){
      ectdStop("No comparator in the code : <, >, >=, <=")
    }
    if( length(gxi) > 2 ){
      ectdStop("Too many comparators (<, >, >=, <=) in the range code")
    }

    out <- switch( length(gxi),
      c( out, code[i]),
      c( out,
        substring(code[i], 1, gxi[2]-1 ),
        substring(code[i], gxi[1] + attr(gxi, "match.length")[1] )) )
  }

  ## paste and parse the code
  out <- paste( "(", out , ")", sep = "", collapse = "&") %-~% "[[:space:]]"
  result <- try( parse( text = out ),  silent = TRUE )
  if( class(result) == "try-error" ) {
    ectdStop('parsing problem: ' %.% ( result %-~% "^[^:]*:" )     )
  }
  result

}

