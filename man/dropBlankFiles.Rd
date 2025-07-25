% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_hip.R
\name{dropBlankFiles}
\alias{dropBlankFiles}
\title{Drop blank files}
\usage{
dropBlankFiles(filelist)
}
\arguments{
\item{filelist}{The file list tibble created by \code{\link{listFiles}}}
}
\description{
The internal \code{dropBlankFiles} function is used inside of
\code{\link{read_hip}} to return an error message if blank files exist in the
directory, and remove them from the file list so they are not read in.
}
\references{
\url{https://github.com/USFWS/migbirdHIP}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
