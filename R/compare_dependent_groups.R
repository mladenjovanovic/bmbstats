#' Dependent groups estimators
#'
#' Function that provides a list of estimators. User is free to define his own list of estimators.
#'        Used in \code{\link{compare_dependent_groups}}
#' @inheritParams basic_arguments
#' @return Named numeric vector with estimators
#' @export
#' @examples
#' data("bench_press_data")
#' pre <- bench_press_data$`Pre-test`
#' post <- bench_press_data$`Post-test`
#' dependent_groups_estimators(pre, post, SESOI_lower = -5, SESOI_upper = 5)
dependent_groups_estimators <- function(pre,
                                          post,
                                          SESOI_lower = 0,
                                          SESOI_upper = 0,
                                          na.rm = FALSE) {
  SESOI_range <- SESOI_upper - SESOI_lower
  change <- post - pre

  mean_change <- mean(change, na.rm = na.rm)
  sd_change <- stats::sd(change, na.rm = na.rm)
  cv_change <- 100 * sd_change / mean_change
  perc_change <- mean(change / pre, na.rm = na.rm) * 100
  ratio <- mean(post / pre, na.rm = na.rm)
  cohen <- cohens_d(pre, post, paired = TRUE, na.rm = na.rm)
  cles <- CLES(pre, post, na.rm = na.rm)
  ovl <- 2 * stats::pnorm(-abs(cohen) / 2)

  change_to_SESOI <- mean_change / SESOI_range
  sd_change_to_SESOI <- sd_change / SESOI_range

  magnitude_proportions <- mb_proportions(
    pre,
    post,
    paired = TRUE,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    method = "algebraic"
  )

  c("SESOI lower" = SESOI_lower,
    "SESOI upper" = SESOI_upper,
    "SESOI range" = SESOI_range,
    "Mean change" = mean_change,
    "SD change" = sd_change,
    "%CV change" = cv_change,
    "% change" = perc_change,
    "Ratio" = ratio,
    "Cohen's d" = cohen,
    "CLES" = cles,
    "OVL" = ovl,
    "Mean change to SESOI" = change_to_SESOI,
    "SD change to SESOI" = sd_change_to_SESOI,
    "pLower" = magnitude_proportions$lower,
    "pEquivalent" = magnitude_proportions$equivalent,
    "pHigher" = magnitude_proportions$higher
  )
}



#' SESOI lower threshold for \code{\link{compare_dependent_groups}}
#'
#' @inheritParams basic_arguments
#'
#' @return SD of \code{pre} multiplied by 0.2 (Cohen's trivial)
#'
#' @export
#' @examples
#' SESOI_lower_dependent_func(rnorm(20), rnorm(10))
SESOI_lower_dependent_func <- function(pre,
                                       post,
                                       na.rm = FALSE) {
  -stats::sd(pre, na.rm = na.rm) * 0.2
}

#' SESOI upper threshold for \code{\link{compare_dependent_groups}}
#'
#' @inheritParams basic_arguments
#'
#' @return SD of \code{pre} multiplied by 0.2 (Cohen's trivial)
#'
#' @export
#' @examples
#' SESOI_upper_dependent_func(rnorm(20), rnorm(10))
SESOI_upper_dependent_func <- function(pre,
                                         post,
                                         na.rm = FALSE) {
  stats::sd(pre, na.rm = na.rm) * 0.2
}

#' Compare two dependent groups
#'
#' \code{compare_dependent_groups} provides numerous descriptive estimators and bootstrap confidence intervals for
#'     comparison between paired \code{pre} and \code{post}. \code{compare_dependent_groups} function is a "wrapper"
#'     for \code{\link{bmbstats}} function.
#' @inheritParams basic_arguments
#' @inheritParams bmbstats
#' @param estimator_function Function that takes \code{pre}, \code{post}, and \code{na.rm} parameters
#'    and return named numeric vector with parameter estimates. Default is \code{\link{dependent_groups_estimators}}.
#'    User can write their own function with needed estimators for which bootstrap confidence intervals
#'    are needed
#' @export
#' @examples
#' data("bench_press_data")
#' pre <- bench_press_data$`Pre-test`
#' post <- bench_press_data$`Post-test`
#' compare_dependent_groups(pre, post, SESOI_lower = -5, SESOI_upper = 5)
compare_dependent_groups <- function(pre,
                                       post,
                                       SESOI_lower = SESOI_lower_dependent_func,
                                       SESOI_upper = SESOI_upper_dependent_func,
                                       estimator_function = dependent_groups_estimators,
                                       control = model_control(),
                                       na.rm = FALSE) {


  if (length(pre) != length(post)) {
    stop("pre and post differ in size. Unable to proceed", call. = FALSE)
  }

  # Bind data
  data <- data.frame(
    pre = pre,
    post = post
  )

  # ----------------------------------------------------
  # Wrapper functions
  bmbstats_SESOI_lower_function <- function(data, na.rm, init_boot) {
    pre <- data$pre
    post <- data$post

    func_num(
      SESOI_lower,
      pre = pre,
      post = post,
      na.rm = na.rm
    )
  }

  bmbstats_SESOI_upper_function <- function(data, na.rm, init_boot) {
    pre <- data$pre
    post <- data$post

    func_num(
      SESOI_upper,
      pre = pre,
      post = post,
      na.rm = na.rm
    )
  }

  bmbstats_estimator_function <- function(data, SESOI_lower, SESOI_upper, na.rm, init_boot) {
    pre <- data$pre
    post <- data$post

    estimators_list <- estimator_function(
      pre = pre,
      post = post,
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
