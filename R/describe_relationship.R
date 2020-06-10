#'  Dependent groups linear relationship estimators
#'
#' Function that provides a list of estimators for linear relationship between \code{predictor} and \code{outcome}.
#'   User is free to define his own list of estimators. Used in \code{\link{compare_dependent_groups}}
#' @inheritParams basic_arguments
#' @return Named numeric vector with estimators
#' @export
#' @examples
#' data("yoyo_mas_data")
#' predictor <- yoyo_mas_data$YoYoIR1
#' outcome <- yoyo_mas_data$MAS
#'
#' relationship_lm_estimators(predictor, outcome, SESOI_lower = -0.5, SESOI_upper = 0.5)
relationship_lm_estimators <- function(predictor,
                                           outcome,
                                           SESOI_lower = 0,
                                           SESOI_upper = 0,
                                           na.rm = FALSE) {

  if (length(predictor) != length(outcome)) {
    stop("predictor and outcome differ in size. Unable to proceed", call. = FALSE)
  }

  SESOI_range <- SESOI_upper - SESOI_lower

  lm_model <- stats::lm(outcome ~ predictor)
  intercept <- stats::coef(lm_model)[[1]]
  slope <- stats::coef(lm_model)[[2]]
  rse <- summary(lm_model)$sigma

  pper <- PPER(
    sigma = rse,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    df = length(predictor) - 1)

  pearson_r <- stats::cor(predictor, outcome)
  r_squared <- pearson_r^2

  c("SESOI lower" = SESOI_lower,
    "SESOI upper" = SESOI_upper,
    "SESOI range" = SESOI_range,
    "Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    "Pearson's r" = pearson_r,
    "R Squared" = r_squared,
    "SESOI to RSE" = SESOI_range / rse,
    "PPER" = pper)
}

#' SESOI lower threshold for \code{\link{compare_dependent_groups}}
#'
#' @inheritParams basic_arguments
#'
#' @return SD of \code{outcome} multiplied by 0.2 (Cohen's trivial)
#'
#' @export
#' @examples
#' SESOI_lower_relationship_func(rnorm(20), rnorm(10))
SESOI_lower_relationship_func <- function(predictor,
                                       outcome,
                                       na.rm = FALSE) {
  -stats::sd(outcome, na.rm = na.rm) * 0.2
}

#' SESOI upper threshold for \code{\link{compare_dependent_groups}}
#'
#' @inheritParams basic_arguments
#'
#' @return SD of \code{outcome} multiplied by 0.2 (Cohen's trivial)
#'
#' @export
#' @examples
#' SESOI_upper_relationship_func(rnorm(20), rnorm(10))
SESOI_upper_relationship_func <- function(predictor,
                                       outcome,
                                       na.rm = FALSE) {
  stats::sd(outcome, na.rm = na.rm) * 0.2
}


#' Describe relationship between two dependent groups
#'
#' \code{describe_relationship} provides numerous descriptive estimators and bootstrap confidence intervals for
#'     relationship between paired \code{predictor} and \code{outcome} groups. \code{describe_relationship} function is a "wrapper"
#'     for \code{\link{bmbstats}} function.
#' @inheritParams basic_arguments
#' @inheritParams bmbstats
#' @param estimator_function Function that takes \code{predictor}, \code{outcome}, and \code{na.rm} parameters
#'    and return named numeric vector with parameter estimates. Default is \code{\link{relationship_lm_estimators}}.
#'    User can write their own function with needed estimators for which bootstrap confidence intervals
#'    are needed
#' @export
#' @examples
#' data("yoyo_mas_data")
#' predictor <- yoyo_mas_data$YoYoIR1
#' outcome <- yoyo_mas_data$MAS
#'
#' describe_relationship(predictor, outcome, SESOI_lower = -0.5, SESOI_upper = 0.5)
describe_relationship <- function(predictor,
                                     outcome,
                                     SESOI_lower = SESOI_lower_dependent_func,
                                     SESOI_upper = SESOI_upper_dependent_func,
                                     estimator_function = relationship_lm_estimators,
                                     control = model_control(),
                                     na.rm = FALSE) {


  if (length(predictor) != length(outcome)) {
    stop("predictor and outcome differ in size. Unable to proceed", call. = FALSE)
  }

  # Bind data
  data <- data.frame(
    predictor = predictor,
    outcome = outcome
  )

  # ----------------------------------------------------
  # Wrapper functions
  bmbstats_SESOI_lower_function <- function(data, na.rm, init_boot) {
    predictor <- data$predictor
    outcome <- data$outcome

    func_num(
      SESOI_lower,
      predictor = predictor,
      outcome = outcome,
      na.rm = na.rm
    )
  }

  bmbstats_SESOI_upper_function <- function(data, na.rm, init_boot) {
    predictor <- data$predictor
    outcome <- data$outcome

    func_num(
      SESOI_upper,
      predictor = predictor,
      outcome = outcome,
      na.rm = na.rm
    )
  }

  bmbstats_estimator_function <- function(data, SESOI_lower, SESOI_upper, na.rm, init_boot) {
    predictor <- data$predictor
    outcome <- data$outcome

    estimators_list <- estimator_function(
      predictor = predictor,
      outcome = outcome,
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
