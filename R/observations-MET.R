#' Minimum-effect tests using observations and known measurement error
#'
#' @inheritParams basic_arguments
#' @param observations Numeric vector of observed scores
#' @param observations_label Vector used for labeling observations. Default is \code{NULL}
#' @param measurement_error Estimated SD of the random error. Default is 0
#' @param df Degrees of freedom from the reliability study where \code{measurement_error} is
#'     estimated. Default is \code{Inf}, which implies the use of normal distribution
#'
#' @export
#'
#' @examples
#' data("bench_press_data")
#' obs_MET <- observations_MET(
#'   bench_press_data$Change,
#'   SESOI_lower = -5,
#'   SESOI_upper = 5,
#'   measurement_error = 2.5,
#'   alpha = 0.05
#' )
#'
#' obs_MET
#' plot(obs_MET)
observations_MET <- function(observations,
                             observations_label = NULL,
                             measurement_error = 0,
                             df = Inf,
                             SESOI_lower = 0,
                             SESOI_upper = 0,
                             alpha = 0.05,
                             confidence = 0.95) {

  # TOST / Equivalence
  tost_upper <- stats::pt((observations - SESOI_upper) / measurement_error, df = df)
  tost_lower <- 1 - stats::pt((observations - SESOI_lower) / measurement_error, df = df)
  equivalence <- pmax(tost_upper, tost_lower)

  # Minimum-Effect tests
  superior <- 1 - stats::pt((observations - SESOI_upper) / measurement_error, df = df)
  non_inferior <- 1 - stats::pt((observations - SESOI_lower) / measurement_error, df = df)
  inferior <- stats::pt((observations - SESOI_lower) / measurement_error, df = df)
  non_superior <- stats::pt((observations - SESOI_upper) / measurement_error, df = df)

  alpha <- alpha * rep(1, length(observations))

  confidence <- confidence * rep(1, length(observations))

  # Final Inference
  final_inference <- ifelse(
    equivalence < alpha,
    "Equivalent",
    ifelse(
      superior < alpha,
      "Higher",
      ifelse(
        inferior < alpha,
        "Lower",
        ifelse(
          non_superior < alpha,
          "Not-Higher",
          ifelse(
            non_inferior < alpha,
            "Not-Lower",
            "Equivocal"
          )
        )
      )
    )
  )

  SDC <- measurement_error * stats::qt(
    1 - ((1 - confidence) / 2),
    df = df
  )
  observations_lower <- observations - SDC
  observations_upper <- observations + SDC

  final_inference <- factor(
    final_inference,
    levels = c(
      "Equivocal",
      "Lower",
      "Not-Higher",
      "Equivalent",
      "Not-Lower",
      "Higher"
    ),
    ordered = TRUE
  )

  if (is.null(observations_label)) {
    observations_label <- seq(observations)
    observations_label <- factor(observations_label)
  }

  inference_label <- paste0(
    final_inference,
    " [",
    ifelse(inferior < alpha, "*", "-"),
    ifelse(non_superior < alpha, "*", "-"),
    ifelse(equivalence < alpha, "*", "-"),
    ifelse(non_inferior < alpha, "*", "-"),
    ifelse(superior < alpha, "*", "-"),
    "]"
  )

  new_observations_MET(
    observations_label = observations_label,
    observations = observations,
    measurement_error = measurement_error,
    df = df,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    lower = inferior,
    not_higher = non_superior,
    equivalent = equivalence,
    not_lower = non_inferior,
    higher = superior,
    alpha = alpha,
    inference = final_inference,
    inference_label = inference_label,
    SDC = SDC,
    confidence = confidence,
    observations_lower = observations_lower,
    observations_upper = observations_upper
  )
}
