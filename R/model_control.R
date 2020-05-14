#' Model Control Constructor
#' @param seed Random number seed
#' @export
model_control <- function(seed = round(stats::runif(1, 1, 10000))){

  list(
    seed = seed
  )
}
