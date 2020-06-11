new_observations_MBI <- function(observations_label,
                                 observations,
                                 measurement_error,
                                 df,
                                 SESOI_lower,
                                 SESOI_upper,
                                 lower,
                                 equivalent,
                                 higher,
                                 inference,
                                 inference_label,
                                 SDC,
                                 confidence,
                                 observations_lower,
                                 observations_upper) {

  observations_MBI_object <- list(
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

  class(observations_MBI_object) <- "bmbstats_observations_MBI"

  return(observations_MBI_object)

}
