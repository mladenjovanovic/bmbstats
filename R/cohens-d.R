#' Calculates pooled SD
#'
#' \code{sd_pooled} calculates pooled standard deviation from vectors \code{group_a} and \code{group_b}
#' @inheritParams basic_arguments
#' @return Pooled SD
#' @export
#' @examples
#' sd_pooled(rnorm(10), rnorm(10))
sd_pooled <- function(group_a, group_b, na.rm = FALSE) {
  a_n <- length(group_a) - 1
  b_n <- length(group_b) - 1

  a_var <- stats::var(group_a, na.rm = na.rm)
  b_var <- stats::var(group_b, na.rm = na.rm)

  return(sqrt(((a_var * a_n) + (b_var * b_n)) / (a_n + b_n)))
}

#' Cohen's d for paired (dependent) and unpaired (independent) samples
#'
#' @inheritParams basic_arguments
#' @param na.rm Should missing values be removed? Default is \code{FALSE}. With \code{paired=TRUE}, NA removal is \emph{NOT} pairwise
#'     and involves NA removal from the difference score and \code{group_a}
#' @return Cohen's d
#' @export
#' @examples
#' cohens_d(rnorm(100), rnorm(100))
cohens_d <- function(group_a, group_b, paired = FALSE, na.rm = FALSE) {
  if (paired) {
    # Paired
    if (length(group_a) != length(group_b)) {
      stop("Group A and Group B differ in size. Unable to proceed")
    }

    group_difference <- group_b - group_a
    if (na.rm) { # Remove NAs (this removal is not pairwise)
      group_difference <- stats::na.omit(group_difference)
      group_a <- stats::na.omit(group_a)
    }
    return(mean(group_difference) / stats::sd(group_a))
  } else {
    # Independent
    if (na.rm) { # Remove NAs
      group_a <- stats::na.omit(group_a)
      group_b <- stats::na.omit(group_b)
    }
    return((mean(group_b) - mean(group_a)) / sd_pooled(group_a, group_b, na.rm = na.rm))
  }
}
