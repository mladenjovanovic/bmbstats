#' Bootstrap minimum effect tests (METs)
#'
#' @param bmbstats_object Object of class \code{bmbstats}
#' @param estimator Name of the estimator from the \code{bmbstats_object}
#' @inheritParams basic_arguments
#' @return \code{bmbstats_MET} object
#' @export
#' @examples
#' mean_MET <- bootstrap_MET(
#'   describe_data(rnorm(10, 100, 20)),
#'   estimator = "mean",
#'   SESOI_lower = 95,
#'   SESOI_upper = 105,
#'   alpha = 0.05
#' )
#' mean_MET
#' plot(mean_MET)
bootstrap_MET <- function(bmbstats_object,
                          estimator,
                          SESOI_lower = 0,
                          SESOI_upper = 0,
                          alpha = 0.05) {

  if (class(bmbstats_object) != "bmbstats") {
    stop("Please provide bmbstats object!", call. = FALSE)
  }

  if (SESOI_lower == SESOI_upper) {
    warning("SESOI thresholds are equal.", immediate. = TRUE, call. = FALSE)
  }

  # TOST / Equivalence test
  equivalence_lower_test <- bootstrap_NHST(
    bmbstats_object, estimator,
    SESOI_lower,
    test = "greater"
  )

  equivalence_upper_test <- bootstrap_NHST(
    bmbstats_object,
    estimator,
    SESOI_upper,
    test = "less"
  )

  # This will be used later for plotting (assumed null distributions)
  SESOI_lower_distribution <- equivalence_lower_test$distribution$null_distribution
  SESOI_upper_distribution <- equivalence_upper_test$distribution$null_distribution

  # Extract estimator info from equivalence_lower_test
  # This is because those values are returned and are the same
  # across all METs
  estimator_value <- equivalence_lower_test$estimator$value
  estimator_lower <- equivalence_lower_test$estimator$lower
  estimator_upper <- equivalence_lower_test$estimator$upper

  confidence <- equivalence_lower_test$estimator$confidence

  # Only keep p_values
  equivalence_lower <- equivalence_lower_test$result$p_value
  equivalence_upper <- equivalence_upper_test$result$p_value

  # Get higher p_value of the two
  equivalence <- max(
    equivalence_lower,
    equivalence_upper
  )

  # METs
  inferiority <- bootstrap_NHST(
    bmbstats_object,
    estimator,
    SESOI_lower,
    test = "less"
  )$result$p_value

  non_superiority <- bootstrap_NHST(
    bmbstats_object,
    estimator,
    SESOI_upper,
    test = "less"
  )$result$p_value

  non_inferiority <- bootstrap_NHST(
    bmbstats_object,
    estimator,
    SESOI_lower,
    test = "greater"
  )$result$p_value

  superiority <- bootstrap_NHST(
    bmbstats_object,
    estimator,
    SESOI_upper,
    test = "greater"
  )$result$p_value

  # Final Inference
  final_inference <- ifelse(equivalence < alpha,
                            "Equivalent",
                            ifelse(superiority < alpha,
                                   "Higher",
                                   ifelse(inferiority < alpha,
                                          "Lower",
                                          ifelse(non_superiority < alpha,
                                                 "Not-Higher",
                                                 ifelse(non_inferiority < alpha,
                                                        "Not-Lower",
                                                        "Equivocal"
                                                 )
                                          )
                                   )
                            )
  )

  # Create return object
  new_bootstrap_MET(
    estimator = list(
      name = estimator,
      value = estimator_value,
      lower = estimator_lower,
      upper = estimator_upper,
      confidence = confidence
    ),
    test = list(
      name = "MET",
      test = c(
        "inferiority",
        "non-superiority",
        "equivalence",
        "non-inferiority",
        "superiority"
      ),
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper,
      alpha = alpha
    ),
    result = list(
      p_value = c(
        inferiority,
        non_superiority,
        equivalence,
        non_inferiority,
        superiority
      ),
      inference = final_inference
    ),
    distribution = list(
      SESOI_lower_distribution = SESOI_lower_distribution,
      SESOI_upper_distribution = SESOI_upper_distribution
    )
  )
}
