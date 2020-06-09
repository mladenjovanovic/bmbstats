#' Independent groups estimators
#'
#' Function that provides a list of estimators. User is free to define his own list of estimators.
#'        Used in \code{\link{compare_independent_groups}}
#' @inheritParams basic_arguments
#' @return Named numeric vector with estimators
#' @export
#' @examples
#' data("height_data")
#' group_a <- height_data$Height[height_data$Gender == "Female"]
#' group_b <- height_data$Height[height_data$Gender == "Male"]
#' independent_groups_estimators(group_a, group_b, SESOI_lower = -2.5, SESOI_upper = 2.5)
independent_groups_estimators <- function(group_a,
                                          group_b,
                                          SESOI_lower = 0,
                                          SESOI_upper = 0,
                                          na.rm = FALSE) {
  SESOI_range <- SESOI_upper - SESOI_lower
  mean_diff <- mean(group_b, na.rm = na.rm) - mean(group_a, na.rm = na.rm)
  sd_diff <- sqrt(stats::var(group_b) + stats::var(group_a))
  sd_pool <- sd_pooled(group_a, group_b, na.rm = na.rm)
  cv_diff <- 100 * sd_diff / mean_diff
  perc_diff <- (mean(group_b, na.rm = na.rm) - mean(group_a, na.rm = na.rm)) / mean(group_a, na.rm = na.rm) * 100
  ratio <- mean(group_b, na.rm = na.rm) / mean(group_a, na.rm = na.rm)
  cohen <- cohens_d(group_a, group_b, paired = FALSE, na.rm = na.rm)
  cles <- CLES(group_a, group_b, na.rm = na.rm)
  ovl <- 2 * stats::pnorm(-abs(cohen) / 2)

  difference_to_SESOI <- mean_diff / SESOI_range
  sd_diff_to_SESOI <- sd_diff / SESOI_range

  magnitude_proportions <- mb_proportions(
    group_a,
    group_b,
    paired = FALSE,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    method = "algebraic"
  )

  c("SESOI lower" = SESOI_lower,
    "SESOI upper" = SESOI_upper,
    "SESOI range" = SESOI_range,
    "Mean diff" = mean_diff,
    "SD diff" = sd_diff,
    "SD pooled" = sd_pool,
    "%CV diff" = cv_diff,
    "% diff" = perc_diff,
    "Ratio" = ratio,
    "Cohen's d" = cohen,
    "CLES" = cles,
    "OVL" = ovl,
    "Mean diff to SESOI" = difference_to_SESOI,
    "SD diff to SESOI" = sd_diff_to_SESOI,
    "pLower" = magnitude_proportions$lower,
    "pEquivalent" = magnitude_proportions$equivalent,
    "pHigher" = magnitude_proportions$higher
  )
}

#' SESOI lower threshold for \code{\link{compare_independent_groups}}
#'
#' @inheritParams basic_arguments
#'
#' @return Pooled SD of \code{group_a} and \code{group_b}
#'     multiplied by -0.2 (Cohen's trivial)
#'
#' @export
#' @examples
#' SESOI_lower_independent_func(rnorm(20), rnorm(10))
SESOI_lower_independent_func <- function(group_a,
                                         group_b,
                                         na.rm = FALSE) {
  -sd_pooled(group_a, group_b, na.rm = na.rm) * 0.2
}

#' SESOI upper threshold for \code{\link{compare_independent_groups}}
#'
#' @inheritParams basic_arguments
#'
#' @return Pooled SD of \code{group_a} and \code{group_b}
#'     multiplied by 0.2 (Cohen's trivial)
#'
#' @export
#' @examples
#' SESOI_upper_independent_func(rnorm(20), rnorm(10))
SESOI_upper_independent_func <- function(group_a,
                                         group_b,
                                         na.rm = FALSE) {
  sd_pooled(group_a, group_b, na.rm = na.rm) * 0.2
}
#' Compare two independent groups
#'
#' \code{compare_independent_groups} provides numerous descriptive estimators and bootstrap confidence intervals for
#'     comparison between \code{group_a} and \code{group_b}. \code{compare_independent_groups} function is a "wrapper"
#'     for \code{\link{bmbstats}} function.
#' @inheritParams basic_arguments
#' @inheritParams bmbstats
#' @param estimator_function Function that takes \code{group_a}, \code{group_b}, and \code{na.rm} parameters
#'    and return named numeric vector with parameter estimates. Default is \code{\link{independent_groups_estimators}}.
#'    User can write their own function with needed estimators for which bootstrap confidence intervals
#'    are needed
#' @export
#' @examples
#' data("height_data")
#' group_a <- height_data$Height[height_data$Gender == "Female"]
#' group_b <- height_data$Height[height_data$Gender == "Male"]
#' compare_independent_groups(group_a, group_b, SESOI_lower = -2.5, SESOI_upper = 2.5)
compare_independent_groups <- function(group_a,
                                       group_b,
                                       SESOI_lower = SESOI_lower_independent_func,
                                       SESOI_upper = SESOI_upper_independent_func,
                                       estimator_function = independent_groups_estimators,
                                       control = model_control(),
                                       na.rm = FALSE) {

  # Bind data
  data <- rbind(
    data.frame(group = "group_a", value = group_a),
    data.frame(group = "group_b", value = group_b)
  )

  # Set strata
  control$boot_strata <- factor(data$group)

  # ----------------------------------------------------
  # Wrapper functions
  bmbstats_SESOI_lower_function <- function(data, na.rm, init_boot) {
    group_a <- data$value[data$group == "group_a"]
    group_b <- data$value[data$group == "group_b"]

    func_num(
      SESOI_lower,
      group_a = group_a,
      group_b = group_b,
      na.rm = na.rm
    )
  }

  bmbstats_SESOI_upper_function <- function(data, na.rm, init_boot) {
    group_a <- data$value[data$group == "group_a"]
    group_b <- data$value[data$group == "group_b"]

    func_num(
      SESOI_upper,
      group_a = group_a,
      group_b = group_b,
      na.rm = na.rm
    )
  }

  bmbstats_estimator_function <- function(data, SESOI_lower, SESOI_upper, na.rm, init_boot) {
    group_a <- data$value[data$group == "group_a"]
    group_b <- data$value[data$group == "group_b"]

    estimators_list <- estimator_function(
      group_a = group_a,
      group_b = group_b,
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper,
      na.rm = na.rm
    )

    return(estimators_list)
  }

  # ---------------------------------------------
  # Call bmbstats
 bmbstats(
    data = data,
    SESOI_lower_function = bmbstats_SESOI_lower_function,
    SESOI_upper_function = bmbstats_SESOI_upper_function,
    estimator_function = bmbstats_estimator_function,
    control = control,
    na.rm = na.rm
  )

}
