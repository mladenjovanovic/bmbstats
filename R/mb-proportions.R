#' Magnitude-based Proportions for Two Independent Groups
#'
#' \code{mb_proportions_independent} calculates lower/equivalent/higher
#'      difference proportions between two independent groups
#' @inheritParams basic_arguments
#' @param method Select "brute-force" or "algebraic" method to calculate proportions. Default is "algebraic"
#' @param use_normal_distribution When estimating proportions algebraically, should normal or t-distribution
#'    be used. Default is TRUE (normal distribution)
#' @return Data frame with lower, equivalent and higher columns
mb_proportions_independent <- function(group_a,
                                       group_b,
                                       SESOI_lower = 0,
                                       SESOI_upper = 0,
                                       method = "algebraic",
                                       na.rm = FALSE,
                                       use_normal_distribution = TRUE) {

  # Check method
  rlang::arg_match(method, c("algebraic", "brute-force"))

  if (na.rm) {
    group_a <- stats::na.omit(group_a)
    group_b <- stats::na.omit(group_b)
  }

  if (use_normal_distribution) {
    df <- Inf
  } else {
    df <- length(group_a) + length(group_b) - 2
  }

  if (method == "algebraic") {
    group_difference <- mean(group_b) - mean(group_a)
    difference_SD <- sqrt(stats::var(group_a) + stats::var(group_b))

    # Calculate proportion of lower/equivalent/higher differences
    higher <- 1 - stats::pt((SESOI_upper - group_difference) / difference_SD, df = df)
    lower <- stats::pt((SESOI_lower - group_difference) / difference_SD, df = df)
    equivalent <- 1 - (higher + lower)

    # higher <- 1 - pnorm(SESOI_upper, group_difference, sd = difference_SD)
    # lower  <- pnorm(SESOI_lower, group_difference, sd = difference_SD)
    # equivalent <- 1 - (higher + lower)

    # Combine the results
    results_DF <- data.frame(
      lower = lower,
      equivalent = equivalent,
      higher = higher
    )

    # row.names(results_DF)[1] <- "algebraic"
    return(results_DF)
  } else { # Method brute-force

    combinations <- expand.grid(group_a = group_a, group_b = group_b)
    differences <- combinations$group_b - combinations$group_a

    # Count observations
    higher <- mean(differences > SESOI_upper)
    lower <- mean(differences < SESOI_lower)
    equivalent <- mean((differences >= (SESOI_lower)) & (differences <= SESOI_upper))

    # Combine the results
    results_DF <- data.frame(
      lower = lower,
      equivalent = equivalent,
      higher = higher
    )

    # row.names(results_DF) <- "brute-force"
    return(results_DF)
  }
}

#' Magnitude-based Proportions for Two Dependent Groups
#'
#' \code{mb_proportions_dependent} calculates lower/equivalent/higher
#'    difference proportions between two dependent groups
#' @inheritParams basic_arguments
#' @inheritParams mb_proportions_independent
#' @return Data frame with lower, equivalent and higher columns
mb_proportions_dependent <- function(group_a,
                                     group_b,
                                     SESOI_lower = 0,
                                     SESOI_upper = 0,
                                     method = "algebraic",
                                     na.rm = FALSE,
                                     use_normal_distribution = TRUE) {
  # Check method
  rlang::arg_match(method, c("algebraic", "brute-force"))

  if (length(group_a) != length(group_b)) {
    stop("group_a and group_b differ in size. Unable to proceed")
  }

  difference <- group_b - group_a

  if (na.rm) {
    difference <- stats::na.omit(difference)
  }

  if (use_normal_distribution) {
    df <- Inf
  } else {
    df <- length(difference) - 1
  }

  if (method == "algebraic") {
    group_difference <- mean(difference)
    difference_SD <- stats::sd(difference)

    # Calculate proportion of lower/equivalent/higher differences
    higher <- 1 - stats::pt((SESOI_upper - group_difference) / difference_SD, df = df)
    lower <- stats::pt((SESOI_lower - group_difference) / difference_SD, df = df)
    equivalent <- 1 - (higher + lower)

    # higher <- 1 - pnorm(SESOI_upper, group_difference, sd = difference_SD)
    # lower  <- pnorm(SESOI_lower, group_difference, sd = difference_SD)
    # equivalent <- 1 - (higher + lower)

    # Combine the results
    results_DF <- data.frame(
      lower = lower,
      equivalent = equivalent,
      higher = higher
    )

    # row.names(results_DF)[1] <- "algebraic"
    return(results_DF)
  } else { # Method brute-force

    # Just use differences

    # Count observations
    higher <- mean(difference > SESOI_upper)
    lower <- mean(difference < (SESOI_lower))
    equivalent <- mean((difference >= (SESOI_lower)) & (difference <= SESOI_upper))

    # Combine the results
    results_DF <- data.frame(
      lower = lower,
      equivalent = equivalent,
      higher = higher
    )

    # row.names(results_DF) <- "brute-force"
    return(results_DF)
  }
}

#' Magnitude-based Proportions for Two Groups
#'
#' \code{mb_proportions} calculates lower/equivalent/higher difference
#'     proportions between two groups
#'
#' @inheritParams basic_arguments
#' @inheritParams mb_proportions_independent
#' @return Data frame with lower, equivalent and higher columns
#' @export
#' @examples
#' mb_proportions(rnorm(100), rnorm(100))
mb_proportions <- function(group_a,
                           group_b,
                           paired = FALSE,
                           SESOI_lower = 0,
                           SESOI_upper = 0,
                           method = "algebraic",
                           na.rm = FALSE,
                           use_normal_distribution = TRUE) {

  # Check method
  rlang::arg_match(method, c("algebraic", "brute-force"))

  if (paired) { # Dependent (Paired)
    mb_proportions_dependent(
      group_a = group_a,
      group_b = group_b,
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper,
      method = method,
      na.rm = na.rm,
      use_normal_distribution = use_normal_distribution
    )
  } else { # Independent
    mb_proportions_independent(
      group_a = group_a,
      group_b = group_b,
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper,
      method = method,
      na.rm = na.rm,
      use_normal_distribution = use_normal_distribution
    )
  }
}
