% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_magnitude}
\alias{get_magnitude}
\title{Get Magnitude}
\usage{
get_magnitude(
  x,
  SESOI_lower,
  SESOI_upper,
  labels = c("Lower", "Equivalent", "Higher")
)
}
\arguments{
\item{x}{Numeric vector}

\item{SESOI_lower}{Lower smallest effect size of interest threshold}

\item{SESOI_upper}{Upper smallest effect size of interest threshold}

\item{labels}{Character vector with three elements. Default is "Lower", "Equivalent", "Higher"}
}
\value{
Ordered factor
}
\description{
Create a factor with labels using SESOI thresholds
}
\examples{
get_magnitude(rnorm(20), -1, 1)
}
