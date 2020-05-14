#' Define \code{\link[ggplot2]{ggplot}} summary function to return mean +/- SD HORIZONTAL
#'
#' @param x Numeric vector
#' @return Data frame with xmin, x, and xmax summaries
#' @export
#' @examples
#' mean_sd_h(rnorm(100))
mean_sd_h <- function(x) {
  x <- stats::na.omit(x)
  standard_deviation <- stats::sd(x)
  average <- mean(x)

  data.frame(
    xmin = average - standard_deviation,
    x = average,
    xmax = average + standard_deviation
  )
}

#' Define  \code{\link[ggplot2]{ggplot}} summary function to return mean +/- SD VERTICAL
#'
#' @param x Numeric vector
#' @return Data frame with xmin, x, and xmax summaries
#' @export
#' @examples
#' mean_sd_v(rnorm(100))
mean_sd_v <- function(x) {
  x <- stats::na.omit(x)
  standard_deviation <- stats::sd(x)
  average <- mean(x)

  data.frame(
    ymin = average - standard_deviation,
    y = average,
    ymax = average + standard_deviation
  )
}

#' Finding Modes Using Kernel Density Estimates
#'
#' @source Website page \url{https://rmflight.github.io/post/finding-modes-using-kernel-density-estimates/}
#' @param x Numeric vector
#' @return Mode estimate
#' @export
#' @examples
#' density_mode(rnorm(100))
density_mode <- function(x) {
  density_estimate <- stats::density(x)

  mode_value <- density_estimate$x[which.max(density_estimate$y)]
  mode_value
}

#' Get Magnitude
#'
#' Create a factor with labels using SESOI thresholds
#' @param x Numeric vector
#' @inheritParams basic_arguments
#' @param labels Character vector with three elements. Default is "Lower", "Equivalent", "Higher"
#' @return Ordered factor
#' @export
#' @examples
#' get_magnitude(rnorm(20), -1, 1)
get_magnitude <- function(x,
                          SESOI_lower,
                          SESOI_upper,
                          labels = c("Lower", "Equivalent", "Higher")) {
  factor(
    ifelse(
      x > SESOI_upper,
      labels[3],
      ifelse(x < SESOI_lower,
        labels[1],
        labels[2]
      )
    ),
    levels = labels,
    ordered = TRUE
  )
}

#' Function or numeric
#' Check if the parameter \code{x} is a function, or numeric. If it is a function,
#'      then parameters \code{...} are forwarded to \code{x}. If it is a numeric, then
#'      the \code{x} is returned
#' @param x Function or numeric
#' @param ... Forwarded to \code{x} if function
#' @return If function \code{x{...}} is returned. If numeric \code{x} is returned
func_num <- function(x, ...) {
  if (is.function(x)) {
    x(...)
  } else {
    x
  }
}
