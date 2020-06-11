#' Bootstrap magnitude based inference
#'
#' Calculates proportion of the estimator bootstrap resample lower/equivalent/higher distribution compared to SESOI band
#'
#' @param bmbstats_object Object of class \code{bmbstats}
#' @param estimator Name of the estimator from the \code{bmbstats_object}
#' @inheritParams basic_arguments
#' @return \code{bmbstats_MBI} object
#' @export
#' @examples
#' mean_MBI <- bootstrap_MBI(
#'   describe_data(rnorm(10, 100, 20)),
#'   estimator = "mean",
#'   SESOI_lower = 95,
#'   SESOI_upper = 105
#' )
#' mean_MBI
#' plot(mean_MBI, control = plot_control(points_jitter = FALSE))
bootstrap_MBI <- function(bmbstats_object,
                          estimator,
                          SESOI_lower = 0,
                          SESOI_upper = 0) {
  if (class(bmbstats_object) != "bmbstats") {
    stop("Please provide bmbstats object!", call. = FALSE)
  }


  if (SESOI_lower == SESOI_upper) {
    warning("SESOI thresholds are equal.", call. = FALSE, immediate. = TRUE)
  }

  ## Estimator ----------------------------
  # Check if there is estimator by that name
  estimator_names <- names(bmbstats_object$boot$t0)
  estimator_location <- which(estimator_names == estimator)

  if (length(estimator_location) != 1) {
    stop("None or multiple estimators by the estimator variable name. Please use the name of the estimator contained in the bmbstats_object",
      call. = FALSE
    )
  }

  # Get the estimates
  estimator_boot_values <- bmbstats_object$boot$t[, estimator_location] # Resamples
  estimator_value <- bmbstats_object$estimators$value[estimator_location]
  estimator_lower <- bmbstats_object$estimators$lower[estimator_location]
  estimator_upper <- bmbstats_object$estimators$upper[estimator_location]

  confidence <- bmbstats_object$control$confidence

  # Calculate proportions
  lower <- mean(estimator_boot_values < SESOI_lower)
  higher <- mean(estimator_boot_values > SESOI_upper)
  equivalent <- 1 - (lower + higher)

  highest_probability <- max(c(lower, equivalent, higher))
  highest_effect <- c("lower", "equivalent", "higher")[which.max(c(lower, equivalent, higher))]

  inference_text <- ifelse(highest_probability < 0.005, "Most unlikely",
    ifelse(highest_probability < 0.05, "Very unlikely",
      ifelse(highest_probability < 0.25, "Unlikely",
        ifelse(highest_probability < 0.75, "Possibly",
          ifelse(highest_probability < 0.95, "Likely",
            ifelse(highest_probability < 0.99, "Most likely",
              ifelse(highest_probability >= 0.99, "Almost certainly")
            )
          )
        )
      )
    )
  )


  inference <- ifelse(lower >= 0.05 & higher >= 0.05,
    "Unclear difference",
    paste(inference_text, highest_effect)
  )

  # Create return objects
  new_bootstrap_MBI(
    estimator = list(
      name = estimator,
      value = estimator_value,
      lower = estimator_lower,
      upper = estimator_upper,
      confidence = confidence
    ),
    test = list(
      name = "MBI",
      test = c("lower", "equivalent", "higher"),
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper
    ),
    result = list(
      p_value = c(lower, equivalent, higher),
      inference = inference
    ),
    distribution = list(estimator_distribution = data.frame(estimator = estimator_boot_values))
  )
}
