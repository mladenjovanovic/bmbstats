% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{mean_sd_h}
\alias{mean_sd_h}
\title{Horizontal Mean +/- SD}
\usage{
mean_sd_h(x)
}
\arguments{
\item{x}{Numeric vector}
}
\value{
Data frame with xmin, x, and xmax summaries
}
\description{
Define \code{\link[ggplot2]{ggplot}} summary function to return mean +/- SD HORIZONTAL
}
\examples{
mean_sd_h(rnorm(100))
}
