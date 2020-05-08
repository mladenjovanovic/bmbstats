#' Common Language Effect Size (CLES)
#'
#' \code{CLES} calculates the probability that a random observation from Group B is higher/larger
#'     than a random observation from Group A. Groups are considered independent.
#'
#' @param group_a Vector of observations from Group A. This groups represents baseline/control
#' @param group_b Vector of observations from Group B.
#' @param SESOI_lower Smallest Effect Size Of Interest. Uses to differentiate between lower/equivalent difference. Default is 0
#' @param SESOI_upper Smallest Effect Size Of Interest. Uses to differentiate between equivalent/higher difference. Default is 0
#' @param method Select "brute-force" or "algebraic" method to calculate CLES. Default is "algebraic"
#' @param count_equivalent How should equivalent differences be counted? Ignored, counted as higher or lower difference.
#'     Default is "ignore". Other options are "higher" and "lower"
#' @param na.rm Should NAs be removed? Default is FALSE
#' @return Numeric
#' @export
#' @examples
#' CLES(
#'   rnorm(n = 50, mean = 100, sd = 10),
#'   rnorm(n = 40, mean = 98, sd = 8)
#' )
CLES <- function(group_a,
                 group_b,
                 SESOI_lower = 0,
                 SESOI_upper = 0,
                 method = "algebraic",
                 count_equivalent = "ignore",
                 na.rm = FALSE) {
  if (na.rm) {
    group_a <- stats::na.omit(group_a)
    group_b <- stats::na.omit(group_b)
  }

  if (method == "algebraic") {
    group_difference <- mean(group_b) - mean(group_a)
    difference_SD <- sqrt(stats::var(group_a) + stats::var(group_b))

    # Calculate proportion of lower/equivalent/higher differences
    higher <- 1 - stats::pnorm(SESOI_upper, group_difference, sd = difference_SD)
    lower <- stats::pnorm(SESOI_lower, group_difference, sd = difference_SD)
    equivalent <- 1 - (higher + lower)

    # Estiamte proabilities of the higher score depending on how to treat equivalent score
    if (count_equivalent == "ignore") {
      algebraic <- (higher / (higher + lower))
    }

    if (count_equivalent == "lower") {
      algebraic <- (higher / (higher + lower + equivalent))
    }

    if (count_equivalent == "higher") {
      algebraic <- ((higher + equivalent) / (higher + lower + equivalent))
    }

    # names(algebraic) <- "algebraic"
    return(algebraic)
  } else { # Method brute-force

    combinations <- expand.grid(group_a = group_a, group_b = group_b)
    differences <- combinations$group_b - combinations$group_a

    # Count observations
    higher <- differences > SESOI_upper
    lower <- differences < SESOI_lower
    equivalent <- (differences >= SESOI_lower) & (differences <= SESOI_upper)

    # Calculate the probability of the higher score based on how to treat equivalent score
    if (count_equivalent == "ignore") {
      brute_force <- sum(higher) / (sum(higher) + sum(lower))
    }

    if (count_equivalent == "lower") {
      brute_force <- sum(higher) / (sum(higher) + sum(lower) + sum(equivalent))
    }

    if (count_equivalent == "higher") {
      brute_force <- (sum(higher) + sum(equivalent)) / (sum(higher) + sum(lower) + sum(equivalent))
    }

    # names(brute_force) <- "brute-force"
    return(brute_force)
  }
}
