#' Unit/System Tests suite for the MSToolkit package
#'
#' This function enables the user of the MSToolkit package to run the unit
#' tests of the package.
#'
#' The unit tests are based on the RUnit framework.
#'
#' @param htmlreport (Optional) A logical value.  Print an html report for unit
#' and system tests?  FALSE by default
#' @param showdetails (Optional) A logical value.  Print the details in the
#' text summary of the unit tests?  TRUE by default
#' @return The function does not return anything. It writes two files:
#' \code{reportUT.html} for the unit tests and \code{reportST.html} for the
#' system tests.
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso \code{\link[RUnit]{runTestSuite}}
#' @references Matthias Burger, Klaus Juenemann and Thomas Koenig (2007).
#' RUnit: R Unit test framework. R package version 0.4.17.
#' https://sourceforge.net/projects/runit/
#'
#' RUnit and R CMD check
#' http://wiki.r-project.org/rwiki/doku.php?id=developers:runit
#' @keywords error
#' @examples
#'
#'   \dontrun{
#'     mstoolkitUnitTests( htmlhelp = TRUE )
#'
#'     # unit tests
#'     browseURL("reportUT.html")
#'
#'     # system tests
#'     browseURL("reportST.html")
#'
#'   }
#'
#'
"mstoolkitUnitTests" <- function(htmlreport = FALSE, showdetails =  TRUE){

  oldverbose <- getEctdVerbose()
  setEctdVerbose( FALSE )

  ## unit tests will not be done if RUnit is not available
  if(require("RUnit", quietly=TRUE)) {

    pkg <- "MSToolkit"

    assign( "unitTestPath"  ,  system.file(package=pkg, "Runit")     , pos = .GlobalEnv )
    assign( "systemTestPath",  system.file(package=pkg, "systemTest"), pos = .GlobalEnv )

    errorLog <- file( "errors.log", open = "wt" )
    sink( errorLog,  type = "message" )
    sink( errorLog )
	unitTestPath <- get("unitTestPath", pos = .GlobalEnv )
	systemTestPath <- get("systemTestPath", pos = .GlobalEnv )
    testSuite <- defineTestSuite( name=paste(pkg, "unit testing"), dirs=unitTestPath)
    tests <- runTestSuite(testSuite)

    testSuiteST <- defineTestSuite( name=paste(pkg, "system testing"), dirs=systemTestPath)
    testsST <- runTestSuite(testSuiteST)

    sink()
    sink(type = "message")
    close(errorLog)

    rm( "unitTestPath"  , pos = .GlobalEnv)
    rm( "systemTestPath", pos = .GlobalEnv)

    cat("------------------- UNIT TEST SUMMARY ---------------------\n\n")
    printTextProtocol(tests, showDetails=showdetails, fileName = "unitTests.txt")
    .formatUnitTest("unitTests.txt")
    pathReport <- file.path( "reportUT")
    if( htmlreport) {
      printHTMLProtocol(tests, fileName=paste(pathReport, ".html", sep="") )
      cat("\nWriting html report in `", pathReport, ".html`\n", sep ="")
    }
    cat("\n\n------------------ SYSTEM TEST SUMMARY ---------------------\n\n")
    printTextProtocol(testsST, showDetails=showdetails, fileName = "systemTests.txt")
    .formatUnitTest("systemTests.txt")
    pathReportST <- file.path("reportST")
    if( htmlreport) {
      printHTMLProtocol(testsST, fileName=paste(pathReportST, ".html", sep="") )
      cat("\nWriting report in `", pathReportST, ".html`\n", sep ="")
    }

  } else {
    warning("cannot run unit tests -- package RUnit is not available")
  }

  setEctdVerbose( oldverbose )
  invisible(NULL)

}



".formatUnitTest" <- function(file = "unitTests.txt") {
  txtlines <- readLines(file)
  sapply( strsplit( txtlines, ": ... OK" , fixed= TRUE ), function(li){

    if(length(li) ==0 ) return(NULL)
    if( length(li) == 1 ) {
      cat(li, "\n")
    }  else if(length(li == 2)){
      li[1] <- li[1] %.% ": OK"
      n1 <- nchar( li[1] )
      n2 <- nchar( li[2] )
      out <- if( n1 + n2 < 80 ) {
        paste( li[1], li[2], sep = paste(rep(" ", 80 - n1-n2), collapse ="",sep="") )
      } else {
        paste( li[1], "\n",sprintf("%80s", li[2]), sep = "" )
      }
      cat( out, "\n")
    }
  } )
  invisible(NULL)
}


