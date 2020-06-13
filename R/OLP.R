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
OLP_regression <- function(predictor, outcome, na.rm = FALSE) {
  if (na.rm) {
    na.idx <- is.na(predictor) | is.na(outcome)
    predictor <- predictor[!na.idx]
    outcome <- outcome[!na.idx]
  }

  slope <- sign(stats::cov(predictor, outcome)) * stats::sd(outcome) / stats::sd(predictor)
  int <- mean(outcome) - slope * mean(predictor)

  y_pred <- int + slope * predictor
  rse <- sqrt((sum((y_pred - outcome)^2)) / (length(outcome) - 2))

  list(
    intercept = int,
    slope = slope,
    rse = rse
  )
}
