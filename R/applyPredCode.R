#' Parses and applys NONMEM statements
#'
#' Parses NONMEM statements and attempts to apply them to a data frame in order
#' to create a response variable
#'
#' The \code{applyPredCode} function calls the \code{parsePredCode} function in
#' order to convert the NONMEM statements to executable R statements.  The
#' \code{applyPredCode} function then iteratively "tries" to apply these
#' statements to the data ("df"), producing a textual report of the process if
#' "report" is set to TRUE.  The "keepCols" columns from the updated dataset
#' are then returned
#'
#' @param df (Required) Data frame to which parsed NONMEM statements are to be
#' applied
#' @param pred Character vector of NONMEM statements
#' @param respCol Response column name ( iven by \link{getEctdColName} by
#' default)
#' @param report Logical: should a textual report be produced?
#' @param keepCols Character vector of column names to retain in the return
#' dataset
#' @param verbose Logical: Should verbose logging be used? ( given by
#' \link{getEctdVerbose} by default)
#' @return A dataset with new columns added
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @keywords NONMEM
#' @examples
#' applyPredCode(
#' df = data.frame(X = 1:5, TH1 = rep(1, 5),
#'  TH2 = rep(2, 5), ETA1 = rep(3, 5), EPS1 = rep(4, 5)), 
#'pred = MSToolkit:::parsePredCode(c("TEST = 1","XCOPY = X","TH2COPY = THETA(2)",
#'"Y = XCOPY + LOG(THETA(1)) + THETA(2)**2 + ETA(1) + SQRT(EPS(1)) + 1")),
#'              respCol ="RESP",
#'              report = FALSE, 
#'              keepCols = c("TEST", "XCOPY", "TH2COPY", "RESP"))
#' @export
"applyPredCode" <- function(
		df, 										#@ Dataset within which to apply $PRED statements
		pred, 										#@ Parsed $PRED statements
		respCol = getEctdColName("Response"),		#@ Response column name
		report = TRUE,								#@ Produce textual report
		keepCols = respCol,							#@ Columns to keep in the return data
		verbose = getEctdVerbose()					#@ Verbose report of data progression
){

	# Check data
	if (!is.data.frame(df)) ectdStop("Input must be a data frame")
	outDf <- df <- df [ setdiff(names(df), respCol) ]

	# Pad out text reporting
	ncMax <- max(nchar(pred)) + 1
	addSpace <- sapply(ncMax - nchar(pred), function(N) paste(rep(" ", N), sep="", collapse=""))
	padPred <- paste(pred, addSpace)

	# Loop around the statements
	if (report) cat(paste("\n # Attempting to create Response variable '", respCol, "'\n", sep=""))

	nPred <- length(pred)
	for (i in 1:nPred) {
		if (report) cat(paste("\n > ", padPred[i], " :", sep=""))
		tryCommand <- try(within(outDf, eval(parse(text = pred[i]))), silent = TRUE)
		if (class(tryCommand) == "try-error") {
			if (report) {
				cat("FAILED")
				errorMessage <- substring(tryCommand[1], regexpr(":", tryCommand[1])+1 )
				cat(" (", gsub("\n", "", errorMessage), " )", sep="")
			}
		}
		else {
			if (report) cat("PASSED")
			outDf <- tryCommand
		}
		if (verbose) {
			cat("\n"); print(utils::head(outDf)); cat("\n")
		}
	}

	# Check whether we have created a response
	if (respCol %in% names(outDf)) {
		if ("F" %in% keepCols & "XF" %in% names(outDf)) {
			if (report) cat("\n NOTE: Requested column 'F' - this has been renamed 'XF' in the data")
			keepCols [ keepCols == "F" ] <- "XF"
		}
		keepCols <- union(names(df), keepCols)
		colsThere <- keepCols %in% names(outDf)
		if (!all(colsThere)) {
			whichCols <- paste(keepCols [ !colsThere ], collapse=", ")
			ectdStop(paste("Some columns could not be found:", whichCols))
		}
		outDf <- outDf [ keepCols ]
		if (report) cat(paste("\n\n # Response variable '", respCol, "' was successfully created\n", sep=""))
	}
	else {
		cat("\n")
		ectdStop("Response variable could not be created")
	}
	outDf
}