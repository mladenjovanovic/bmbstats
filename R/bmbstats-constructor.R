new_bmbstats <- function(estimators,
                         estimator_function,
                         SESOI_lower_function,
                         SESOI_upper_function,
                         boot,
                         extra = NULL,
                         control,
                         na.rm) {
  bmbstats_object <- list(
    estimators = estimators,
    estimator_function = estimator_function,
    SESOI_lower_function = SESOI_lower_function,
    SESOI_upper_function = SESOI_upper_function,
    boot = boot,
    extra = extra,
    control = control,
    na.rm = na.rm
  )

  class(bmbstats_object) <- "bmbstats"
  return(bmbstats_object)
}
