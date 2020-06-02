#' Proportion of Practically Equivalent Residuals
#'
#' @inheritParams basic_arguments
#' @param sigma Numeric
#' @param df Degrees of freedom. Used for t-distribution. Default is \code{Inf}
#'
#' @return Numeric
#' @export
#'
#' @examples
#' PPER(0.5, -1, 1)
PPER <- function(sigma, SESOI_lower = 0, SESOI_upper = 0, df = Inf) {
  higher <- 1 - stats::pt((SESOI_upper) / sigma, df = df)
  lower <- stats::pt((SESOI_lower) / sigma, df = df)
  PPER <- 1 - (higher + lower)

  return(PPER)
}
