new_observations_MET <- function(observations_label,
                                 observations,
                                 measurement_error,
                                 df,
                                 SESOI_lower,
                                 SESOI_upper,
                                 lower,
                                 not_higher,
                                 equivalent,
                                 not_lower,
                                 higher,
                                 alpha,
                                 inference,
                                 inference_label,
                                 SDC,
                                 confidence,
                                 observations_lower,
                                 observations_upper) {

  observations_MET_object <- list(
    observations_label = observations_label,
    observations = observations,
    measurement_error = measurement_error,
    df = df,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    lower = lower,
    not_higher = not_higher,
    equivalent = equivalent,
    not_lower = not_lower,
    higher = higher,
    alpha = alpha,
    inference = inference,
    inference_label = inference_label,
    SDC = SDC,
    confidence = confidence,
    observations_lower = observations_lower,
    observations_upper = observations_upper
  )

  class(observations_MET_object) <- "bmbstats_observations_MET"

  return(observations_MET_object)

}
