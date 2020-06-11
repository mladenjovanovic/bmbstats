new_bootstrap_MBI <- function(estimator, test, result, distribution) {
  bootstrap_MBI_object <- list(
    estimator = estimator,
    test = test,
    results = result,
    distribution = distribution
  )

  class(bootstrap_MBI_object) <- "bmbstats_MBI"

  return(bootstrap_MBI_object)
}
