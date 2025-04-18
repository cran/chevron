% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lbt14.R
\docType{data}
\name{lbt14_main}
\alias{lbt14_main}
\alias{lbt14_pre}
\alias{lbt14_post}
\alias{lbt14}
\title{\code{LBT14} Laboratory Test Results Shift Table – Highest \code{NCI-CTCAE} Grade Post-Baseline by
Baseline Grade (Low or High Direction).}
\format{
An object of class \code{chevron_t} of length 1.
}
\usage{
lbt14_main(
  adam_db,
  arm_var = "ACTARM",
  lbl_overall = NULL,
  gr_missing = "incl",
  page_var = "PARAMCD",
  ...
)

lbt14_pre(adam_db, gr_missing = "incl", direction = "low", ...)

lbt14_post(tlg, prune_0 = TRUE, ...)

lbt14
}
\arguments{
\item{adam_db}{(\code{list} of \code{data.frames}) object containing the \code{ADaM} datasets}

\item{arm_var}{(\code{string}) variable used for column splitting}

\item{lbl_overall}{(\code{string}) label used for overall column, if set to \code{NULL} the overall column is omitted}

\item{gr_missing}{(\code{string}) how missing baseline grades should be handled. Defaults to \code{"incl"} to include the
\code{"Missing"}
level. Other options are \code{"excl"} to exclude patients with missing baseline grades and \code{"gr_0"} to convert missing
baseline grades to grade 0.}

\item{page_var}{(\code{string}) variable name prior to which the row split is by page.}

\item{...}{not used.}

\item{direction}{(\code{string}) one of \code{"high"} or \code{"low"} indicating which shift direction should be detailed.}

\item{tlg}{(\code{TableTree}, \code{Listing} or \code{ggplot}) object typically produced by a \code{main} function.}

\item{prune_0}{(\code{flag}) remove 0 count rows}
}
\value{
the main function returns an \code{rtables} object.

the preprocessing function returns a \code{list} of \code{data.frame}.

the postprocessing function returns an \code{rtables} object or an \code{ElementaryTable} (null report).
}
\description{
\code{LBT14} Laboratory Test Results Shift Table – Highest \code{NCI-CTCAE} Grade Post-Baseline by
Baseline Grade (Low or High Direction).
}
\details{
\itemize{
\item This table follows ADaMIG v1.1.
\item Only the worst grade recorded for each patient is included in the table.
\item If no missing baseline lab results, the "Missing" level of \code{BTOXGR} is excluded.
\item Grading takes value from -4 to 4, negative value means the abnormality direction is low,
positive value means the abnormality direction is high.
\item Grades 0, 1, 2, 3, and 4 are counted as \code{"Not Low"} when \code{direction = "low"}. Conversely, when \code{direction =  "high"}, Grades 0, -1, -2, -3, and -4 are counted as `"Not High".
\item Remove zero-count rows unless overridden with \code{prune_0 = FALSE}.
\item Split columns by arm, typically \code{ACTARM}.
}
}
\section{Functions}{
\itemize{
\item \code{lbt14_main()}: Main TLG function

\item \code{lbt14_pre()}: Preprocessing

\item \code{lbt14_post()}: Postprocessing

}}
\note{
\itemize{
\item \code{adam_db} object must contain an \code{adlb} table with columns \code{"USUBJID"}, \code{"PARAM"}, \code{"BTOXGR"}, \code{"ATOXGR"},
and the column specified by \code{arm_var}.
}
}
\examples{
run(lbt14, syn_data)
}
\keyword{datasets}
