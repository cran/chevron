% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assertions.R
\name{assert_valid_variable}
\alias{assert_valid_variable}
\title{Check variables in a data frame are valid character or factor.}
\usage{
assert_valid_variable(
  df,
  vars,
  label = deparse(substitute(df)),
  types = NULL,
  ...
)
}
\arguments{
\item{df}{(\code{data.frame}) input dataset.}

\item{vars}{(\code{character}) variables to check.}

\item{label}{(\code{string}) labels of the data frame.}

\item{types}{Named (\code{list}) of type of the input.}

\item{...}{further arguments for \code{assert_valid_var}. Please note that different methods have different arguments
so if provided make sure the variables to check is of the same class.}
}
\value{
invisible \code{TRUE} or an error message if the criteria are not fulfilled.
}
\description{
Check variables in a data frame are valid character or factor.
}
