% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/errorPlots.R
\name{errorPlotFields}
\alias{errorPlotFields}
\title{Plot HIP errors by field}
\usage{
errorPlotFields(proofed_data, loc = "all", year)
}
\arguments{
\item{proofed_data}{The object created after error flagging data with
\code{\link{proof}} or \code{\link{correct}}}

\item{loc}{The location that errors should be plotted for. Acceptable values
include:
\itemize{
\item "all" - all states
\item a two-letter abbreviation for a US state; one of:
\itemize{
\item AL, AK, AZ, AR, CA, CO, CT, DE, FL, GA, ID, IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY}
}}

\item{year}{The year in which the HIP data were collected.}
}
\description{
Create a bar plot of proportion of error per field. The plot defaults to all
49 states, but location can be specified.
}
\references{
\url{https://github.com/USFWS/migbirdHIP}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
