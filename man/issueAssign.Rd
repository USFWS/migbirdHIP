% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/issuance.R
\name{issueAssign}
\alias{issueAssign}
\title{Assign decisions to records on how to process them using issue dates and
license years}
\usage{
issueAssign(clean_data, year)
}
\arguments{
\item{clean_data}{The object created after cleaning data with
\code{\link{clean}}}

\item{year}{The year of the HIP season (e.g. 2022 for the 2022-2023 season)}
}
\description{
The internal \code{issueAssign} function is used inside of
\code{\link{issueCheck}} to determine which records in the output from
\code{\link{clean}} are current, past, future, or incorrect.
}
\references{
\url{https://github.com/USFWS/migbirdHIP}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
