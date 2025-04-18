% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rspt01.R
\docType{data}
\name{rspt01_main}
\alias{rspt01_main}
\alias{rspt01_pre}
\alias{rspt01_post}
\alias{rspt01}
\title{\code{RSPT01} Binary Outcomes Summary.}
\format{
An object of class \code{chevron_t} of length 1.
}
\usage{
rspt01_main(
  adam_db,
  dataset = "adrs",
  arm_var = "ARM",
  ref_group = NULL,
  odds_ratio = TRUE,
  perform_analysis = "unstrat",
  strata = NULL,
  conf_level = 0.95,
  methods = list(),
  ...
)

rspt01_pre(adam_db, ...)

rspt01_post(tlg, prune_0 = TRUE, ...)

rspt01
}
\arguments{
\item{adam_db}{(\code{list} of \code{data.frames}) object containing the \code{ADaM} datasets}

\item{dataset}{(\code{string}) the name of a table in the \code{adam_db} object.}

\item{arm_var}{(\code{string}) variable used for column splitting}

\item{ref_group}{(\code{string}) The name of the reference group, the value should
be identical to the values in \code{arm_var}, if not specified, it will by default
use the first level or value of \code{arm_var}.}

\item{odds_ratio}{(\code{flag}) should the odds ratio be calculated, default is \code{TRUE}}

\item{perform_analysis}{(\code{string}) option to display statistical comparisons using stratified analyses,
or unstratified analyses, or both, e.g. \code{c("unstrat", "strat")}. Only unstratified will be displayed by default}

\item{strata}{(\code{string}) stratification factors, e.g. \code{strata = c("STRATA1", "STRATA2")}, by default as NULL}

\item{conf_level}{(\code{numeric}) the level of confidence interval, default is 0.95.}

\item{methods}{(\code{list}) a named list, use a named list to control, for example:
\code{methods = list(prop_conf_method = "wald", diff_conf_method = "wald", strat_diff_conf_method = "ha", diff_pval_method = "fisher", strat_diff_pval_method = "schouten")}
\code{prop_conf_method} controls the methods of calculating proportion confidence interval,
\code{diff_conf_method} controls the methods of calculating unstratified difference confidence interval,
\code{strat_diff_conf_method} controls the methods of calculating stratified difference confidence interval,
\code{diff_pval_method} controls the methods of calculating unstratified p-value for odds ratio,
\code{strat_diff_pval_method} controls the methods of calculating stratified p-value for odds ratio,
see more details in \code{tern}}

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
\code{RSPT01} template may be used to summarize any binary outcome or response variable at
a single time point. Typical application for oncology
}
\details{
\itemize{
\item No overall value.
}
}
\section{Functions}{
\itemize{
\item \code{rspt01_main()}: Main TLG function

\item \code{rspt01_pre()}: Preprocessing

\item \code{rspt01_post()}: Postprocessing

}}
\examples{
library(dplyr)
library(dunlin)

proc_data <- log_filter(syn_data, PARAMCD == "BESRSPI", "adrs")

run(rspt01, proc_data)

run(rspt01, proc_data,
  odds_ratio = FALSE, perform_analysis = c("unstrat", "strat"),
  strata = c("STRATA1", "STRATA2"), methods = list(diff_pval_method = "fisher")
)
}
\keyword{datasets}
