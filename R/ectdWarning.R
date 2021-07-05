#' @describeIn ectdStop Error handling functions for the ectd package.
#' @inheritParams base::warning
#'
ectdWarning <- function(..., call. = TRUE, immediate. = FALSE, domain = NULL) {
	warning(..., call. = call., immediate. = immediate., domain = domain)
}

