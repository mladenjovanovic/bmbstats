new_bootstrap_NHST <- function(estimator, test, result, distribution) {

  bootstrap_NHST_object <- list(
    estimator = estimator,
    test = test,
    results = result,
    distribution = distribution
  )

  class(bootstrap_NHST_object) <- "bmbstats_NHST"

  return(bootstrap_NHST_object)
}
