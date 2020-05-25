#' Standardize numeric vector
#'
#' Function standardizes numeric vector using multiple methods
#'
#' @inheritParams basic_arguments
#' @param method Character string. Default is 'zs' (for z-score method).
#'    Other options include '01' (0 to 1 scaling) and 'pr' (percent rank)
#' @return Numeric vector with standardized values
#' @export
#' @examples
#' x <- c(1, 4, 5, 6, 2, 4, 10, 12, 5)
#' standardize(x)
#' standardize(x, method = "01")
#' standardize(x, method = "pr")
standardize <- function(x, method = "zs", na.rm = FALSE) {
  if (!(method %in% c("zs", "01", "pr"))) {
    stop("Please use one of the following methods: 'zw' for z-score; '01' for 0 to 1 scaling, 'pr' for percent rank", call. = FALSE)
  }

  # Remove NAs
  if (na.rm) {
    x <- stats::na.omit(x)
  }
  x_std <- NULL

  if (method == "zs") {
    x_std <- (x - mean(x)) / stats::sd(x)
  }

  if (method == "01") {
    x_std <- (x - min(x)) / ((max(x) - min(x)))
  }

  if (method == "pr") {
    x_std <- dplyr::percent_rank(x)
  }

  return(x_std)
}
