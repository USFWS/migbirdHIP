% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nDropped.R
\name{nDropped}
\alias{nDropped}
\title{Summary table of how many registrations were dropped}
\usage{
nDropped(raw_data, clean_data, current_data, deduplicated_data, year)
}
\arguments{
\item{raw_data}{The object created after reading in data with
\code{\link{read_hip}}}

\item{clean_data}{The object created after cleaning data with
\code{\link{clean}}}

\item{current_data}{The object created after filtering to current data with
\code{\link{issueCheck}}}

\item{deduplicated_data}{The object created after deduplicating data with
\code{\link{duplicateFix}}}

\item{year}{The year of the HIP season (e.g. 2022 for the 2022-2023 season)}
}
\description{
Internal function that summarizes the number of dropped registrations.
}
\references{
\url{https://github.com/USFWS/migbirdHIP}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
