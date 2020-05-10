#' OLP regression
#'
#'
#' @inheritParams basic_arguments
#' @return List with the following elements: \code{intercept}, \code{slope}, \code{rse}
#' @source Function modified using the function from
#'    \url{https://stat.ethz.ch/pipermail/r-help/2011-July/285022.html}
#' @export
#' @examples
#' data("weight_data")
#' with(
#'   weight_data,
#'   OLP_regression(`OS 1`, `OS 3`)
#' )
OLP_regression <- function(group_a, group_b, na.rm = FALSE) {

  if (na.rm) {
    na.idx <- is.na(group_a) | is.na(group_b)
    group_a <- group_a[!na.idx]
    group_b <- group_b[!na.idx]
  }

  slope <- sign(stats::cov(group_a, group_b)) * stats::sd(group_b) / stats::sd(group_a)
  int <- mean(group_b) - slope * mean(group_a)

  y_pred <- int + slope * group_a
  rse <- sqrt((sum((y_pred - group_b)^2)) / (length(group_b) - 2))

  list(
    intercept = int,
    slope = slope,
    rse = rse
  )
}
