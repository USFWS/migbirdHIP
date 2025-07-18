% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/duplicates.R
\name{duplicateFields}
\alias{duplicateFields}
\title{Find causes of duplication}
\usage{
duplicateFields(duplicates, fields)
}
\arguments{
\item{duplicates}{The tibble created by \code{\link{duplicateID}}}

\item{fields}{Name of the columns to compare values for. One or more of
the fields from the following list may be supplied:
\itemize{
\item title, firstname, middle, lastname, suffix, address, city, state, zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag, dove_bag, woodcock_bag, coots_snipe, rails_gallinules, cranes, band_tailed_pigeon, brant, seaducks, registration_yr, email}}
}
\description{
The internal \code{duplicateFields} function is used inside of
\code{\link{duplicateFinder}} to find which fields have different values
among a group of duplicate registrations.
}
\references{
\url{https://github.com/USFWS/migbirdHIP}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
