#' Data estimators
#'
#' Function that provides a list of estimators. User is free to define his own list of estimators.
#'        Used in \code{\link{describe_data}}
#' @inheritParams basic_arguments
#' @return Named numeric vector with estimators
#' @export
#' @examples
#' data_estimators(iris$Sepal.Width)
data_estimators <- function(x, na.rm = FALSE) {
  if (na.rm) x <- stats::na.omit(x)

  c(
    mean = mean(x),
    SD = stats::sd(x),
    `CV %` = (stats::sd(x) / mean(x)) * 100,
    median = stats::median(x),
    mode = density_mode(x),
    MAD = stats::mad(x),
    IQR = stats::IQR(x),
    min = min(x),
    max = max(x),
    range = max(x) - min(x)
  )
}

#' Data estimators (simple)
#'
#' Function that provides a simple list of estimators (mean and SD).
#'     User is free to define his own list of estimators. Used in \code{\link{describe_data}}
#'
#' @inheritParams basic_arguments
#' @return Named numeric vector with estimators \code{\link[base]{mean}}, and \code{\link[stats]{sd}}
#' @export
#' @examples
#' data_estimators_simple(iris$Sepal.Width)
data_estimators_simple <- function(x, na.rm = FALSE) {
  if (na.rm) x <- stats::na.omit(x)

  c(
    mean = mean(x),
    SD = stats::sd(x)
  )
}

#' Data estimators (robust)
#'
#' Function that provides a list of robust estimators. User is free to define his own list of estimators.
#'     Used in \code{\link{describe_data}}
#'
#' @inheritParams basic_arguments
#' @return Named numeric vector with estimators
#' @export
#' @examples
#' data_estimators_robust(iris$Sepal.Width)
data_estimators_robust <- function(x, na.rm = FALSE) {
  if (na.rm) x <- stats::na.omit(x)

  c(
    median = stats::median(x),
    MAD = stats::mad(x),
    IQR = stats::IQR(x),
    "10% trimmed mean" = mean(x, trim = 0.1),
    "20% trimmed mean" = mean(x, trim = 0.2)
  )
}

#' Describe data
#'
#' \code{describe_data} provides numerous descriptive estimators and bootstrap confidence intervals for
#'     variable \code{x}. \code{describe_data} function is a "wrapper" for \code{\link{bmbstats}} function.
#'
#' @inheritParams basic_arguments
#' @inheritParams bmbstats
#' @param estimator_function Function that takes \code{x} and \code{na.rm} parameters and return named
#'     numeric vector with parameter estimates. Default is \code{\link{data_estimators}}. Other functions
#'     that could be used are \code{\link{data_estimators_simple}} and \code{\link{data_estimators_robust}}.
#'     User can write their own function with needed estimators for which bootstrap confidence intervals
#'     are needed
#' @export
#' @examples
#' desc1 <- describe_data(
#'   iris$Sepal.Width,
#'   estimator_function = data_estimators_simple,
#'   control = model_control(confidence = 0.90, boot_samples = 1000))
#'
#' desc1
#' plot(desc1)
describe_data <- function(x,
                          estimator_function = data_estimators,
                          control = model_control(),
                          na.rm = FALSE) {
  df <- data.frame(value = x)

  # Wrapper for estimation function
  bmbstats_estimator_function <- function(data,
                                          SESOI_lower,
                                          SESOI_upper,
                                          na.rm,
                                          init_boot) {
    # Call function in describe_data environment

    # For some reason, boot::boot function returns vector when DF has only one columns
    # That's why I use data, rather than data$value (creates error)
    estimator_function(
      x = data,
      na.rm = na.rm
    )
  }

  results <- bmbstats(
    data = df,
    estimator_function = bmbstats_estimator_function,
    control = control,
    na.rm = na.rm
  )

  return(results)
}
