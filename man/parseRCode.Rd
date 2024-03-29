% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseRCode.R
\name{parseRCode}
\alias{parseRCode}
\title{Parse R Code}
\usage{
parseRCode(code)
}
\arguments{
\item{code}{Character string containing R Code to parse}
}
\value{
An \link{expression} object
}
\description{
Parses character string to ensure it is valid R code, returning an R
"expression" object
}
\details{
The \code{parseRCode} function wraps "parse" and checks to ensure a valid
expression is created
}
\author{
Romain Francois, Rich Pugh
}
