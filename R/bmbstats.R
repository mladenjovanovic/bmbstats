#' Bootstrap Magnitude-based generic function
#'
#' This is the "core" function of the \code{bmbstats} package.
#'     It performs bootstrap on the `data` using provided `estimator_function`,
#'     `SESOI_lower_function`, and `SESOI_upper_function`. Used in other functions ("wrappers")
#'
#' @param data Data frame
#' @param SESOI_lower_function Function to estimate SESOI_lower within bootstrap loop.
#'     Default functions is \code{function(data, na.rm, init_boot) {return(0)}}
#' @param SESOI_upper_function Function to estimate SESOI_upper within bootstrap loop.
#'     Default functions is \code{function(data, na.rm, init_boot) {return(0)}}
#' @param estimator_function Function to be used within the bootstrap loop to provide a list
#'     or named numeric vector of parameters.
#'     Default is \code{function(data, SESOI_lower, SESOI_upper, na.rm, init_boot){return(0)}}
#' @param control Control object returned from \code{\link{model_control}} function.
#'     Use \code{boot_type}, \code{boot_samples}, \code{boot_strata} to setup bootstrap.
#' @param na.rm Should missing values be removed? Default is \code{FALSE}.
#'     This is forwarded to \code{estimator_function}, \code{SESOI_lower_function}, and \code{SESOI_upper_function}
#' @return A `bmbstats` object
#' @export
#' @examples
#' bmbstats(iris,
#'   SESOI_lower_function = function(data, na.rm, init_boot) {
#'     sd(data$Sepal.Length) * -0.2
#'   },
#'   SESOI_upper_function = function(data, na.rm, init_boot) {
#'     sd(data$Sepal.Length) * 0.2
#'   },
#'   estimator_function = function(data, SESOI_lower, SESOI_upper, na.rm, init_boot) {
#'     list(mean = mean(data$Sepal.Length), SESOI_lower = SESOI_lower, SESOI_upper = SESOI_upper)
#'   },
#'   control = model_control(boot_type = "perc", boot_samples = 50)
#' )
bmbstats <- function(data,
                     SESOI_lower_function = function(data, na.rm, init_boot) {
                       return(0)
                     },
                     SESOI_upper_function = function(data, na.rm, init_boot) {
                       return(0)
                     },
                     estimator_function = function(data, SESOI_lower, SESOI_upper, na.rm, init_boot) {
                       return(0)
                     },
                     control = model_control(),
                     na.rm = FALSE) {

    # Progress bar
  # Show progress bar
  if (control$iter) {
    pb <- progress::progress_bar$new(
      total = control$boot_samples + 1,
      format = "(:spin) [:bar] :percent eta: :eta"
    )
    pb$tick(0)
  }

  # Bootstrap function
  bs <- function(data, indices) {
    if (control$iter) pb$tick()

    # allows boot to select sample
    data <- data[indices, ]

    # Find out if this is the initial boot sample (estimate of parameters)
    init_boot <- identical(
      indices,
      seq(1, length(indices))
    )

    #################
    # Get SESOI values
    boot_SESOI_lower <- SESOI_lower_function(
      data = data,
      na.rm = na.rm,
      init_boot = init_boot
    )

    boot_SESOI_upper <- SESOI_upper_function(
      data = data,
      na.rm = na.rm,
      init_boot = init_boot
    )


    ############################
    # Estimators
    estimators_list <- estimator_function(
      data,
      SESOI_lower = boot_SESOI_lower,
      SESOI_upper = boot_SESOI_upper,
      init_boot = init_boot,
      na.rm = na.rm
    )

    if ((control$iter & init_boot)) {
      message(paste("Bootstraping: ", control$boot_samples, " resamples", sep = ""))
    }

    ############################
    # Convert returned list to a vector
    boot_results <- unlist(estimators_list)

    return(boot_results)
  }

  #####################################
  # Main
  #####################################

  # Set-up seed for reproducibility
  set.seed(control$seed)

  # Check if strata is NULL
  if (is.null(control$boot_strata)) {
    control$boot_strata <- rep(1, nrow(data))
  }


  # Perform boot
  results <- boot::boot(
    data = data,
    statistic = bs,
    R = control$boot_samples,
    strata = control$boot_strata
  )

  # Get CIs
  if (control$iter) message("Calculating confidence intervals...")
  bootrap_confidence_intervals <- get_bootstrap_ci(
    boot_object = results,
    control = control
  )

  new_bmbstats(
    estimators = bootrap_confidence_intervals,
    estimator_function = estimator_function,
    SESOI_lower_function = SESOI_lower_function,
    SESOI_upper_function = SESOI_upper_function,
    boot = results,
    control = control,
    na.rm = na.rm
    )
}


# ========================================================================
#' Gets bootstrap confidence intervals
#'
#' Function to extract confidence intervals from bootstrap object
#'
#' @param boot_object Bootstrap object returned from \code{link[boot]{boot}} function
#' @param control Control object returned from \code{\link{model_control}} function.
#' @return Data frame
get_bootstrap_ci <- function(boot_object,
                             control = model_control()) {

  estimator_names <- names(boot_object$t0)
  n_estimators <- length(estimator_names)

  # Data frame for saving CIs
  boot_ci <- data.frame(
    estimator = factor(estimator_names, levels = estimator_names),
    value = boot_object$t0,
    lower = NA,
    upper = NA
  )

  for (i in seq(1, n_estimators)) {

    # Try to get boot CIs
    boot_parameter_ci <- tryCatch(
      error = function(cnd) {
        warning(
          paste0(
            "boot::boot.ci returned error or NULL when estimating CIs for ", estimator_names[[i]], " estimator. ",
            "Returning NAs for upper and lower CIs"),
          call. = FALSE
        )
        return(NULL)
      },
      {
      boot_ci_return <- boot::boot.ci(
        boot_object,
        type = control$boot_type,
        index = i,
        conf = control$confidence)

      if(is.null(boot_ci_return)) stop()
        boot_ci_return
      }
    )
    if (!is.null(boot_parameter_ci)) {
      # This works only for 'perc' and 'bca' methods of calculating CIs
      boot_ci$lower[i] <- boot_parameter_ci[[4]][4]
      boot_ci$upper[i] <- boot_parameter_ci[[4]][5]
      }
  }

  rownames(boot_ci) <- NULL
  return(boot_ci)
}
