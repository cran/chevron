% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtables_utils.R
\name{occurrence_lyt}
\alias{occurrence_lyt}
\title{Occurrence Layout}
\usage{
occurrence_lyt(
  arm_var,
  lbl_overall,
  row_split_var,
  lbl_row_split,
  medname_var,
  lbl_medname_var,
  summary_labels,
  count_by
)
}
\arguments{
\item{arm_var}{(\code{string}) variable used for column splitting}

\item{lbl_overall}{(\code{string}) label used for overall column, if set to \code{NULL} the overall column is omitted}

\item{row_split_var}{(\code{character}) additional row split variables.}

\item{medname_var}{(\code{string}) variable name of medical treatment name.}

\item{lbl_medname_var}{(\code{string}) label for the variable defining the medication name.}

\item{summary_labels}{(\code{list}) of summarize labels. See details.}
}
\description{
Occurrence Layout
}
\keyword{internal}
