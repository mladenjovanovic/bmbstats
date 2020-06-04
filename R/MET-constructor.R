new_bootstrap_MET <- function(estimator, test, result, distribution) {

  bootstrap_MET_object <- list(
    estimator = estimator,
    test = test,
    results = result,
    distribution = distribution
  )

  class(bootstrap_MET_object) <- "bmbstats_MET"

  return(bootstrap_MET_object)
}
