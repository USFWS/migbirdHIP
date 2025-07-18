% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/errorPlots.R
\name{redFlags}
\alias{redFlags}
\title{Pull bad data}
\usage{
redFlags(proofed_data, type, threshold = 0)
}
\arguments{
\item{proofed_data}{A proofed data table created by \code{\link{proof}}}

\item{type}{Type of tibble to report. Acceptable values include:
\itemize{
\item state
\item field
}}

\item{threshold}{Value above which errors should be tabulated}
}
\description{
Create a tibble of error data by state or field. Data are reported using a
threshold of proportion of error.
}
\references{
\url{https://github.com/USFWS/migbirdHIP}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
