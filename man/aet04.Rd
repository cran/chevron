% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aet04.R
\docType{data}
\name{aet04_main}
\alias{aet04_main}
\alias{aet04_pre}
\alias{aet04_post}
\alias{aet04}
\title{\code{AET04} Table 1 (Default) Adverse Events by Highest \code{NCI} \code{CTACAE} \code{AE} Grade Table 1.}
\format{
An object of class \code{chevron_t} of length 1.
}
\usage{
aet04_main(
  adam_db,
  arm_var = "ACTARM",
  lbl_overall = NULL,
  grade_groups = NULL,
  ...
)

aet04_pre(adam_db, ...)

aet04_post(tlg, prune_0 = TRUE, ...)

aet04
}
\arguments{
\item{adam_db}{(\code{list} of \code{data.frames}) object containing the \code{ADaM} datasets}

\item{arm_var}{(\code{string}) variable used for column splitting}

\item{lbl_overall}{(\code{string}) label used for overall column, if set to \code{NULL} the overall column is omitted}

\item{grade_groups}{(\code{list}) putting in correspondence toxicity grades and labels.}

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
The \code{AET04} table provides an
overview of adverse event with the highest \code{NCI} \code{CTCAE} grade per individual.
}
\details{
\itemize{
\item Numbers represent absolute numbers of patients and fraction of \code{N}, or absolute number of event when specified.
\item Remove zero-count rows unless overridden with \code{prune_0 = FALSE}.
\item Events with missing grading values are excluded.
\item Split columns by arm, typically \code{ACTARM}.
\item Does not include a total column by default.
\item Sort Body System or Organ Class and Dictionary-Derived Term by highest overall frequencies. Analysis Toxicity
Grade is sorted by severity.
}
}
\section{Functions}{
\itemize{
\item \code{aet04_main()}: Main TLG function

\item \code{aet04_pre()}: Preprocessing

\item \code{aet04_post()}: Postprocessing

}}
\note{
\itemize{
\item \code{adam_db} object must contain an \code{adae} table with the columns \code{"AEBODSYS"}, \code{"AEDECOD"} and \code{"ATOXGR"}.
}
}
\examples{
grade_groups <- list(
  "Grade 1-2" = c("1", "2"),
  "Grade 3-4" = c("3", "4"),
  "Grade 5" = c("5")
)
proc_data <- dunlin::log_filter(syn_data, AEBODSYS == "cl A.1", "adae")
run(aet04, proc_data, grade_groups = grade_groups)
}
\keyword{datasets}
