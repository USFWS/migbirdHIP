% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{nonDigitBagsFilter}
\alias{nonDigitBagsFilter}
\title{Filter out non-digit bags}
\usage{
nonDigitBagsFilter(raw_data)
}
\arguments{
\item{raw_data}{The object created after reading in data with \code{\link{read_hip}}}
}
\description{
The internal \code{nonDigitBagsFilter} function filters out any record if any bag value is not a 1-digit number.
}
\references{
\url{https://github.com/USFWS/migbirdHIP}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
