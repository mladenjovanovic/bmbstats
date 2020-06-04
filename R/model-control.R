#' Model Control Constructor
#' @param cv_folds Number of cross-validation folds. Default is 5
#' @param cv_repeats Number of cross-validation repeats. Default is 10
#' @param cv_strata Strata to be used for cross-validation. Default is \code{NULL}
#' @param boot_type Type of bootstrap confidence interval ("bca" and "perc"). Default is "bca"
#' @param boot_samples Number of bootstrap resamples. Default is 2000
#' @param boot_strata Strata to be used for bootstrap. Default is \code{NULL}
#' @param confidence Confidence to be used for calculating bootstrap CIs. Default is 0.95
#' @param iter Should progress bar be shown? Default is \code{TRUE}
#' @param seed Random number seed
#' @export
model_control <- function(cv_folds = 5,
                          cv_repeats = 10,
                          cv_strata = NULL,
                          boot_type = "bca",
                          boot_samples = 2000,
                          boot_strata = NULL,
                          confidence = 0.95,
                          iter = TRUE,
                          seed = round(stats::runif(1, 1, 10000))) {
  rlang::arg_match(boot_type, c(
    "bca",
    "perc"
  ))

  list(
    cv_folds = cv_folds,
    cv_repeats = cv_repeats,
    cv_strata = cv_strata,
    boot_type = boot_type,
    boot_samples = boot_samples,
    boot_strata = boot_strata,
    confidence = confidence,
    iter = iter,
    seed = seed
  )
}
