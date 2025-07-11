% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glyphs.R
\name{glyphCheck}
\alias{glyphCheck}
\title{Find non-UTF-8 glyphs/characters in any field}
\usage{
glyphCheck(raw_data)
}
\arguments{
\item{raw_data}{The object created after reading in data with
\code{\link{read_hip}}}
}
\description{
Pull and view any non-UTF-8 characters in the raw data. This function
iterates \code{\link{glyphFinder}} over the entire tibble.
}
\references{
\url{https://github.com/USFWS/migbirdHIP}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
