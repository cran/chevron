% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/egt02.R
\docType{data}
\name{egt02_1_main}
\alias{egt02_1_main}
\alias{egt02_pre}
\alias{egt02_post}
\alias{egt02_1}
\title{\code{EGT02} ECG Abnormalities Table.}
\format{
An object of class \code{chevron_t} of length 1.
}
\usage{
egt02_1_main(
  adam_db,
  arm_var = "ACTARM",
  lbl_overall = NULL,
  exclude_base_abn = FALSE,
  ...
)

egt02_pre(adam_db, ...)

egt02_post(tlg, ...)

egt02_1
}
\arguments{
\item{adam_db}{(\code{list} of \code{data.frames}) object containing the \code{ADaM} datasets}

\item{arm_var}{(\code{string}) variable used for column splitting}

\item{lbl_overall}{(\code{string}) label used for overall column, if set to \code{NULL} the overall column is omitted}

\item{exclude_base_abn}{(\code{flag}) whether baseline abnormality should be excluded.}

\item{...}{not used.}

\item{tlg}{(\code{TableTree}, \code{Listing} or \code{ggplot}) object typically produced by a \code{main} function.}
}
\value{
the main function returns an \code{rtables} object

the preprocessing function returns a \code{list} of \code{data.frame}.

the postprocessing function returns an \code{rtables} object or an \code{ElementaryTable} (null report).
}
\description{
ECG Parameters outside Normal Limits Regardless of Abnormality at Baseline Table.
}
\details{
\itemize{
\item Only count LOW or HIGH values.
\item Results of "LOW LOW" are treated as the same as "LOW", and "HIGH HIGH" the same as "HIGH".
\item Does not include a total column by default.
\item Does not remove zero-count rows unless overridden with \code{prune_0 = TRUE}.
}
}
\section{Functions}{
\itemize{
\item \code{egt02_1_main()}: Main TLG function

\item \code{egt02_pre()}: Preprocessing

\item \code{egt02_post()}: Postprocessing

}}
\note{
\itemize{
\item \code{adam_db} object must contain an \code{adeg} table with the \code{"PARAM"}, \code{"ANRIND"} and \code{"BNRIND"} columns.
}
}
\examples{
run(egt02_1, syn_data)
}
\keyword{datasets}
