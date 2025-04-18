% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtht01.R
\docType{data}
\name{dtht01_main}
\alias{dtht01_main}
\alias{dtht01_pre}
\alias{dtht01_post}
\alias{dtht01}
\title{\code{DTHT01} Table 1 (Default) Death Table.}
\format{
An object of class \code{chevron_t} of length 1.
}
\usage{
dtht01_main(
  adam_db,
  arm_var = "ACTARM",
  lbl_overall = NULL,
  other_category = FALSE,
  time_since_last_dose = FALSE,
  ...
)

dtht01_pre(adam_db, ...)

dtht01_post(tlg, prune_0 = TRUE, ...)

dtht01
}
\arguments{
\item{adam_db}{(\code{list} of \code{data.frames}) object containing the \code{ADaM} datasets}

\item{arm_var}{(\code{string}) variable used for column splitting}

\item{lbl_overall}{(\code{string}) label used for overall column, if set to \code{NULL} the overall column is omitted}

\item{other_category}{(\code{flag}) should the breakdown of the \code{OTHER} category be displayed.}

\item{time_since_last_dose}{(\code{flag}) should the time to event information be displayed.}

\item{...}{not used.}

\item{tlg}{(\code{TableTree}, \code{Listing} or \code{ggplot}) object typically produced by a \code{main} function.}

\item{prune_0}{(\code{flag}) remove 0 count rows}
}
\value{
the main function returns an \code{rtables} object.

the preprocessing function returns a \code{list} of \code{data.frame}.

the postprocessing function returns an \code{rtables} object or an \code{ElementaryTable} (null report).
}
\description{
A description of the causes of death optionally with the breakdown of the
\code{OTHER} category and/or post-study reporting of death.
}
\details{
\itemize{
\item Numbers represent absolute numbers of subjects and fraction of \code{N}, or absolute numbers when specified.
\item Remove zero-count rows unless overridden with \code{prune_0 = FALSE}.
\item Does not include a total column by default.
}
}
\section{Functions}{
\itemize{
\item \code{dtht01_main()}: Main TLG function

\item \code{dtht01_pre()}: Preprocessing

\item \code{dtht01_post()}: Postprocessing

}}
\note{
\itemize{
\item \code{adam_db} object must contain an \code{adsl} table with the columns \code{"DTHFL"}, \code{"DTHCAT"} as well as \code{LDDTHGR1} if
\code{time_since_last_dose} is \code{TRUE}.
}
}
\examples{
run(dtht01, syn_data)

run(dtht01, syn_data, other_category = TRUE, time_since_last_dose = TRUE)
}
\keyword{datasets}
