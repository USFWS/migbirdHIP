% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{permitStrataFix}
\alias{permitStrataFix}
\title{Fix permit strata}
\usage{
permitStrataFix(raw_data)
}
\arguments{
\item{raw_data}{The object created after reading in data with \code{\link{read_hip}}}
}
\description{
The internal \code{permitStrataFix} function is used inside of \code{\link{clean}} to edit strata for states that submit permit files separately from HIP. If records from these states submit a "2" for the band_tailed_pigeon or crane field, they will be mistakenly identified as permit records. The \code{permitStrataFix} function changes band_tailed_pigeon and/or crane "2" values to "0" so that they are classified as HIP records until permit files are received later in the hunting season.
}
\references{
\url{https://github.com/USFWS/migbirdHIP}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
