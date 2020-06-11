#' Bootstrap Null Hypothesis Significance Test
#'
#' @param bmbstats_object Object of class \strong{bmbstats}
#' @param estimator Name of the estimator from the \code{bmbstats_object}
#' @param null_hypothesis Numeric scalar indicating null hypothesis value for \code{estimator}. Default is 0
#' @param test Character string specifying the type of NHST, must be one of "two.sided" (default), "greater" or "less".
#'     Default is "two.sided"
#' @return \code{bmbstats_NHST} object
#'
#' @export
#' @examples
#' mean_NHST <- bootstrap_NHST(
#'   describe_data(rnorm(100)),
#'   "mean"
#' )
#' mean_NHST
#' plot(mean_NHST)
bootstrap_NHST <- function(bmbstats_object,
                           estimator,
                           null_hypothesis = 0,
                           test = "two.sided") {
  if (class(bmbstats_object) != "bmbstats") {
    stop("Please provide bmbstats object!", call. = FALSE)
  }

  rlang::arg_match(test, c(
    "two.sided",
    "greater",
    "less"
  ))

  ## Estimator ----------------------------
  # Check if there is estimator by that name
  estimator_names <- names(bmbstats_object$boot$t0)
  estimator_location <- which(estimator_names == estimator)

  if (length(estimator_location) != 1) {
    stop(
      "None or multiple estimators by the estimator variable name. Please use the name of the estimator contained in the bmbstats_object",
      call. = FALSE
    )
  }

  # Get the estimates
  estimator_boot_values <- bmbstats_object$boot$t[, estimator_location] # Resamples
  estimator_value <- bmbstats_object$estimators$value[estimator_location]
  estimator_lower <- bmbstats_object$estimators$lower[estimator_location]
  estimator_upper <- bmbstats_object$estimators$upper[estimator_location]

  confidence <- bmbstats_object$control$confidence

  # Calculate p_value
  # -----------------------------------------

  # Create assumed null distribution by "shifting" estimator distribution
  # to be around null hypothesis
  null_distribution <- data.frame(
    null_hypothesis = estimator_boot_values + (null_hypothesis - estimator_value),
    estimator = estimator_value
  )

  # This method rates 0.5 to equal values
  null_distribution$greater <- ifelse(
    estimator_value > null_distribution$null_hypothesis,
    1,
    ifelse(
      estimator_value == null_distribution$null_hypothesis,
      0.5,
      0
    )
  )

  null_distribution$less <- ifelse(
    estimator_value < null_distribution$null_hypothesis,
    1,
    ifelse(
      estimator_value == null_distribution$null_hypothesis,
      0.5,
      0
    )
  )

  # Needed for two sided test
  estimator_max <- max(
    estimator_value, null_hypothesis + (null_hypothesis - estimator_value)
  )
  estimator_min <- min(
    estimator_value, null_hypothesis + (null_hypothesis - estimator_value)
  )

  null_distribution$two.sided <- ifelse(
    (estimator_max > null_distribution$null_hypothesis) &
      (estimator_min < null_distribution$null_hypothesis),
    1,
    ifelse(
      estimator_value == null_distribution$null_hypothesis,
      0.5,
      0
    )
  )

  # calculate p_value
  p_value <- NA

  if (test == "greater") {
    p_value <- 1 - mean(null_distribution$greater)
    null_distribution$extreme <- null_distribution$greater
  }

  if (test == "less") {
    p_value <- 1 - mean(null_distribution$less)
    null_distribution$extreme <- null_distribution$less
  }

  if (test == "two.sided") {
    p_value <- 1 - mean(null_distribution$two.sided)
    null_distribution$extreme <- null_distribution$two.sided
  }

  # Adjusting p_value when equal to zero.
  # Suggestion by Guillaume Rousselet (twitter: @robustgar)
  # Link: https://twitter.com/robustgar/status/1204693662065709056
  # Divide 1 by number of bootstrap resamples
  if (p_value == 0) p_value <- 1 / (1 + bmbstats_object$control$boot_samples)

  # delete columns
  null_distribution$less <- NULL
  null_distribution$greater <- NULL
  null_distribution$two.sided <- NULL

  # Create return object
  new_bootstrap_NHST(
    estimator = list(
      name = estimator,
      value = estimator_value,
      lower = estimator_lower,
      upper = estimator_upper,
      confidence = confidence
    ),
    test = list(
      name = "NHST",
      test = test,
      null_hypothesis = null_hypothesis
    ),
    result = list(p_value = p_value),
    distribution = list(null_distribution = null_distribution)
  )
}
