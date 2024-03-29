% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtables_utils.R
\docType{data}
\name{report_null}
\alias{report_null}
\alias{null_report}
\alias{null_listing}
\title{Create a Null Report}
\format{
An object of class \code{ElementaryTable} with 1 rows and 1 columns.

An object of class \code{listing_df} (inherits from \code{tbl_df}, \code{tbl}, \code{data.frame}) with 1 rows and 1 columns.
}
\usage{
report_null(tlg, ...)

null_report

null_listing
}
\arguments{
\item{tlg}{(\code{TableTree}) object.}

\item{...}{not used. Important to be used directly as post processing function.}
}
\value{
original \code{TableTree} or a null report if no observation are found in the table.
}
\description{
Create a Null Report
}
\keyword{datasets}
