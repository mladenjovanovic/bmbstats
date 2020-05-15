#' Model Control Constructor
#' @param cv_folds Number of cross-validation folds. Default is 5
#' @param cv_repeats Number of cross-validation repeats. Default is 10
#' @param cv_strata Strata to be used for cross-validation. Default is \code{NULL}
#' @param iter Should progress bar be shown? Default is \code{TRUE}
#' @param seed Random number seed
#' @export
model_control <- function(cv_folds = 5,
                          cv_repeats = 10,
                          cv_strata = NULL,
                          iter = TRUE,
                          seed = round(stats::runif(1, 1, 10000))){

  list(
    cv_folds = cv_folds,
    cv_repeats = cv_repeats,
    cv_strata = cv_strata,
    iter = iter,
    seed = seed
  )
}
