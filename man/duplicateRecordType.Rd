% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/duplicates.R
\name{duplicateRecordType}
\alias{duplicateRecordType}
\title{Set record type}
\usage{
duplicateRecordType(duplicates)
}
\arguments{
\item{duplicates}{The tibble created by \code{\link{duplicateID}}}
}
\description{
The internal \code{duplicateRecordType} function is used inside of
\code{\link{duplicateFix}} to set record type of registrations based on each
record's bag values.
}
\references{
\url{https://github.com/USFWS/migbirdHIP}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
