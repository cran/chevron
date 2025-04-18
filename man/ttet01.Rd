% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ttet01.R
\docType{data}
\name{ttet01_main}
\alias{ttet01_main}
\alias{ttet01_pre}
\alias{ttet01_post}
\alias{ttet01}
\title{\code{TTET01} Binary Outcomes Summary.}
\format{
An object of class \code{chevron_t} of length 1.
}
\usage{
ttet01_main(
  adam_db,
  dataset = "adtte",
  arm_var = "ARM",
  ref_group = NULL,
  summarize_event = TRUE,
  perform_analysis = "unstrat",
  strata = NULL,
  ...
)

ttet01_pre(adam_db, dataset = "adtte", ...)

ttet01_post(tlg, prune_0 = TRUE, ...)

ttet01
}
\arguments{
\item{adam_db}{(\code{list} of \code{data.frames}) object containing the \code{ADaM} datasets}

\item{dataset}{(\code{string}) the name of a table in the \code{adam_db} object.}

\item{arm_var}{(\code{string}) variable used for column splitting}

\item{ref_group}{(\code{string}) The name of the reference group, the value should
be identical to the values in \code{arm_var}, if not specified, it will by default
use the first level or value of \code{arm_var}.}

\item{summarize_event}{(\code{flag}) should the event description be displayed, default is TRUE}

\item{perform_analysis}{(\code{string}) option to display statistical comparisons using stratified analyses,
or unstratified analyses, or both, e.g. \code{c("unstrat", "strat")}. Only unstratified will be displayed by default}

\item{strata}{(\code{string}) stratification factors, e.g. \code{strata = c("STRATA1", "STRATA2")}, by default as NULL}

\item{...}{Further arguments passed to \code{control_surv_time()}, \code{control_coxph()}, \code{control_survtp()}, and
\code{surv_timepoint()}. For details, see the documentation in \code{tern}. Commonly used arguments include \code{pval_method},
\code{conf_level}, \code{conf_type}, \code{quantiles}, \code{ties}, \code{time_point}, \code{method}, etc.}

\item{tlg}{(\code{TableTree}, \code{Listing} or \code{ggplot}) object typically produced by a \code{main} function.}

\item{prune_0}{(\code{flag}) remove 0 count rows}
}
\value{
the main function returns an \code{rtables} object.

the preprocessing function returns a \code{list} of \code{data.frame}.

the postprocessing function returns an \code{rtables} object or an \code{ElementaryTable} (null report).
}
\description{
\code{TTET01} template may be used to summarize any binary outcome or response variable at
a single time point. Typical application for oncology
}
\details{
\itemize{
\item No overall value.
}
}
\section{Functions}{
\itemize{
\item \code{ttet01_main()}: Main TLG function

\item \code{ttet01_pre()}: Preprocessing

\item \code{ttet01_post()}: Postprocessing

}}
\examples{
library(dplyr)
library(dunlin)

proc_data <- log_filter(syn_data, PARAMCD == "PFS", "adtte")
run(ttet01, proc_data)

run(ttet01, proc_data,
  summarize_event = FALSE, perform_analysis = c("unstrat", "strat"),
  strata = c("STRATA1", "STRATA2"),
  conf_type = "log-log",
  time_point = c(6, 12),
  method = "both"
)
}
\keyword{datasets}
