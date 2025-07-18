% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/duplicates.R
\name{duplicateDecide}
\alias{duplicateDecide}
\title{Decide which duplicate records should be kept or dropped}
\usage{
duplicateDecide(dupes)
}
\arguments{
\item{dupes}{The tibble created by \code{\link{duplicateID}}}
}
\description{
The internal \code{duplicateDecide} function is used inside of
\code{\link{duplicateFix}} to deduplicate intermediate tibbles.
}
\references{
\url{https://github.com/USFWS/migbirdHIP}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
