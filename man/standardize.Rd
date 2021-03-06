% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardize.R
\name{standardize}
\alias{standardize}
\title{Standardize numeric vector}
\usage{
standardize(x, method = "zs", na.rm = FALSE)
}
\arguments{
\item{x}{Numeric vector}

\item{method}{Character string. Default is 'zs' (for z-score method).
Other options include '01' (0 to 1 scaling) and 'pr' (percent rank)}

\item{na.rm}{Should NAs be removed? Default is \code{FALSE}}
}
\value{
Numeric vector with standardized values
}
\description{
Function standardizes numeric vector using multiple methods
}
\examples{
x <- c(1, 4, 5, 6, 2, 4, 10, 12, 5)
standardize(x)
standardize(x, method = "01")
standardize(x, method = "pr")
}
