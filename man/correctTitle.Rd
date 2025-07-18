% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correct.R
\name{correctTitle}
\alias{correctTitle}
\title{Correct title}
\usage{
correctTitle(proofed_data)
}
\arguments{
\item{proofed_data}{The object created after error flagging data with
\code{\link{proof}}}
}
\description{
The internal \code{correctTitle} function is used inside of
\code{\link{correct}} to change the value(s) in the title field to NA if an
error is detected.
}
\references{
\url{https://github.com/USFWS/migbirdHIP}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
