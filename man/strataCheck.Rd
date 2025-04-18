% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strata.R
\name{strataCheck}
\alias{strataCheck}
\title{Check state strata}
\usage{
strataCheck(deduplicated_data)
}
\arguments{
\item{deduplicated_data}{The object created after deduplicating data with \code{\link{duplicateFix}}}
}
\description{
After fixing the data with \code{\link{duplicateFix}}, ensure there are no new strata that have been introduced by a state to the species groups.
}
\references{
\url{https://github.com/USFWS/migbirdHIP}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
