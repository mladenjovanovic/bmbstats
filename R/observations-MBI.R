#' Magnitude-based inference using observations and known measurement error
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
#' obs_MBI <- observations_MBI(
#'   bench_press_data$Change,
#'   SESOI_lower = -5,
#'   SESOI_upper = 5,
#'   measurement_error = 2.5,
#'   confidence = 0.9
#' )
#'
#' obs_MBI
#' plot(obs_MBI)
observations_MBI <- function(observations,
                             observations_label = NULL,
                             measurement_error = 0,
                             df = Inf,
                             SESOI_lower = 0,
                             SESOI_upper = 0,
                             confidence = 0.95) {

  # MBIs
  lower <- stats::pt((SESOI_lower - observations) / measurement_error, df = df)
  higher <- 1 - stats::pt((SESOI_upper - observations) / measurement_error, df = df)
  equivalent <- 1 - (lower + higher)

  confidence <- confidence * rep(1, length(observations))

  # Final Inference
  highest_probability <- pmax(lower, equivalent, higher)

  highest_effect <- c("lower", "equivalent", "higher")[max.col(data.frame(lower, equivalent, higher))]

  inference_text <- ifelse(highest_probability < 0.005, "Most unlikely",
                           ifelse(highest_probability < 0.05, "Very unlikely",
                                  ifelse(highest_probability < 0.25, "Unlikely",
                                         ifelse(highest_probability < 0.75, "Possibly",
                                                ifelse(highest_probability < 0.95, "Likely",
                                                       ifelse(highest_probability < 0.99, "Most likely",
                                                              "Almost certainly")
                                                       )
                                                )
                                         )
                                  )
                           )

  inference <- ifelse(lower >= 0.05 & higher >= 0.05,
                      "Unclear difference",
                      paste(inference_text, highest_effect)
  )

  inference_levels <- expand.grid(
    inference = c(
      "Most unlikely",
      "Very unlikely",
      "Unlikely",
      "Possibly",
      "Likely",
      "Most likely",
      "Almost certainly"),
    effect = c(
      "lower",
      "equivalent",
      "higher"
    )
  )

  inference_levels <- c("Unclear difference", paste(inference_levels$inference, inference_levels$effect))

  inference <- factor(inference, levels = inference_levels)

  SDC <- measurement_error * stats::qt(
    1 - ((1 - confidence) / 2),
    df = df
  )

  observations_lower <- observations - SDC
  observations_upper <- observations + SDC

  if(is.null(observations_label)) {
    observations_label <- seq(observations)
    observations_label <- factor(observations_label)
  }

inference_label <- paste0(
  inference,
  " [",
  round(lower * 100, 0),
  "/",
  round(equivalent * 100, 0),
  "/",
  round(higher * 100, 0),
  "]"
)

  new_observations_MBI(
    observations_label = observations_label,
    observations = observations,
    measurement_error = measurement_error,
    df = df,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    lower = lower,
    equivalent = equivalent,
    higher = higher,
    inference = inference,
    inference_label = inference_label,
    SDC = SDC,
    confidence = confidence,
    observations_lower = observations_lower,
    observations_upper = observations_upper
  )
}
