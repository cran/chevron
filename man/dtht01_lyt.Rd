% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtht01.R
\name{dtht01_lyt}
\alias{dtht01_lyt}
\title{\code{dtht01} Layout}
\usage{
dtht01_lyt(
  arm_var,
  lbl_overall,
  death_flag,
  death_var,
  other_level,
  other_var,
  dose_death_var
)
}
\arguments{
\item{arm_var}{(\code{string}) variable used for column splitting}

\item{lbl_overall}{(\code{string}) label used for overall column, if set to \code{NULL} the overall column is omitted}

\item{death_flag}{(\code{string}) variable name of death flag.}

\item{death_var}{(\code{string}) variable name of death category.}

\item{other_level}{(\code{string}) \code{"Other"} level in death category.}

\item{other_var}{(\code{string}) variable name of death cause under \code{"Other"}.}

\item{dose_death_var}{(\code{string}) variable name of the days from last dose.}
}
\value{
a \code{PreDataTableLayouts} object.
}
\description{
\code{dtht01} Layout
}
\keyword{internal}
