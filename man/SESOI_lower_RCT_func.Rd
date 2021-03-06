% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rct-analysis.R
\name{SESOI_lower_RCT_func}
\alias{SESOI_lower_RCT_func}
\title{SESOI lower threshold for RCT analysis}
\usage{
SESOI_lower_RCT_func(
  control_pre_test,
  control_post_test,
  treatment_pre_test,
  treatment_post_test,
  na.rm = FALSE
)
}
\arguments{
\item{control_pre_test}{Numeric vector containing Control Pre-test observations}

\item{control_post_test}{Numeric vector containing Control Post-test observations}

\item{treatment_pre_test}{Numeric vector containing Treatment Pre-test observations}

\item{treatment_post_test}{Numeric vector containing Treatment Post-test observations}

\item{na.rm}{Should NAs be removed? Default is \code{FALSE}}
}
\value{
Pooled SD of \code{control_pre_test} and \code{treatment_pre_test}
multiplied by -0.2 (Cohen's trivial)
}
\description{
\code{SESOI_lower_RCT_func} is used in \code{\link{RCT_analysis}} function
}
\examples{
set.seed(1666)

SESOI_lower_RCT_func(
  control_pre_test = rnorm(20, 100, 10),
  control_post_test = rnorm(20, 105, 10),
  treatment_pre_test = rnorm(20, 100, 10),
  treatment_post_test = rnorm(20, 120, 10)
)
}
