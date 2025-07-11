% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_hip.R
\name{ignoreHolds}
\alias{ignoreHolds}
\title{Ignore hold files}
\usage{
ignoreHolds(filelist)
}
\arguments{
\item{filelist}{The file list tibble created by \code{\link{listFiles}}}
}
\description{
The internal \code{ignoreHolds} function is used inside of
\code{\link{read_hip}} to filter out hold files from the file list.
}
\references{
\url{https://github.com/USFWS/migbirdHIP}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
