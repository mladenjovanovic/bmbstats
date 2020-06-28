#' Validity estimators
#'
#' \code{validity_estimators} provides validity estimators using the simple linear regression model,
#'    where \code{criterion} is the outcome variable, and \code{practical} is the predictor.
#'
#' @inheritParams basic_arguments
#' @param criterion Character vector indicating column name in the \code{data}
#' @param practical Character vector indicating column name in the \code{data}
#'
#' @return Named vector with validity estimators
#' @export
#'
#' @examples
#' data("agreement_data")
#'
#' validity_estimators(
#'   data = agreement_data,
#'   criterion = "Criterion_score.trial1",
#'   practical = "Practical_score.trial1"
#' )
validity_estimators <- function(data,
                                criterion,
                                practical,
                                SESOI_lower = 0,
                                SESOI_upper = 0,
                                na.rm = FALSE) {

  # Check the length of the criterion and practical
  if (length(criterion) != 1) {
    stop("Criterion must be a single column", call. = FALSE)
  }

  if (length(practical) != 1) {
    stop("Practical must be a single column", call. = FALSE)
  }

  practical_obs <- data[[practical]]
  criterion_obs <- data[[criterion]]

  SESOI_range <- SESOI_upper - SESOI_lower

  lm_model <- stats::lm(criterion_obs~practical_obs)

  rse <- summary(lm_model)$sigma

  pper <- PPER(
    sigma = rse,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    df = length(criterion_obs) - 1
  )

  pearson_r <- stats::cor(practical_obs, criterion_obs)
  r_squared <- pearson_r^2

  sdc <- rse * stats::qt(1 - ((1 - 0.95) / 2), df = length(criterion_obs) - 1)

  c(
    "SESOI lower" = SESOI_lower,
    "SESOI upper" = SESOI_upper,
    "SESOI range" = SESOI_range,
    "Intercept" = stats::coef(lm_model)[[1]],
    "Slope" = stats::coef(lm_model)[[2]],
    "RSE" = rse,
    "Pearson's r" = pearson_r,
    "R Squared" = r_squared,
    "SESOI to RSE" = SESOI_range / rse,
    "PPER" = pper,
    "SDC" = sdc
  )
}


#' Validity Analysis
#'
#' \code{validity_analysis} represents a wrapper function for the \code{\link{bmbstats}} function.
#'     \code{validity_analysis} runs the bootstrap validity analysis of the \code{data} data frame using
#'     \code{estimator_function} to return the estimators
#' @param data Data frame
#' @param criterion Character vector indicating column name in the \code{data}
#' @param practical Character vector indicating column name(s) in the \code{data}
#' @param SESOI_lower Function or numeric scalar. Default is \code{\link{SESOI_lower_validity_func}}
#' @param SESOI_upper Function or numeric scalar. Default is \code{\link{SESOI_upper_validity_func}}
#' @param estimator_function Function for providing validity estimators. Default is \code{\link{validity_estimators}}
#' @param control Control object returned from \code{\link{model_control}} function.
#'     Use \code{boot_type}, \code{boot_samples}, \code{boot_strata}, and \code{confidence} to setup bootstrap.
#' @param na.rm Should NAs be removed? Default is \code{FALSE}
#'
#' @return Object of class `bmbstats`
#' @export
#'
#' @examples
#' data("agreement_data")
#'
#' val_analysis <- validity_analysis(
#'   data = agreement_data,
#'   criterion = "Criterion_score.trial1",
#'   practical = "Practical_score.trial1",
#'   control = model_control(
#'     boot_type = "perc",
#'     boot_samples = 1000,
#'     seed = 1667
#'   )
#' )
#'
#' val_analysis
#'
#' plot(val_analysis)
validity_analysis <- function(data,
                              criterion,
                              practical,
                              SESOI_lower = SESOI_lower_validity_func,
                              SESOI_upper = SESOI_upper_validity_func,
                              estimator_function = validity_estimators,
                              control = model_control(),
                              na.rm = FALSE) {

  # --------------------------------------------
  # Wrapper functions
  bmbstats_SESOI_lower_function <- function(data, na.rm, init_boot) {
    func_num(
      SESOI_lower,
      data = data,
      criterion = criterion,
      practical = practical,
      na.rm = na.rm
    )
  }

  bmbstats_SESOI_upper_function <- function(data, na.rm, init_boot) {
    func_num(
      SESOI_upper,
      data = data,
      criterion = criterion,
      practical = practical,
      na.rm = na.rm
    )
  }

  # ------------------------------------------
  bmbstats_estimator_function <- function(data,
                                          SESOI_lower,
                                          SESOI_upper,
                                          na.rm,
                                          init_boot) {
    estimators_list <- estimator_function(
      data = data,
      criterion = criterion,
      practical = practical,
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper,
      na.rm = na.rm
    )

    return(estimators_list)
  }

  # --------------------------------------------
  # Call bmbstats
  bmbstats(
    data = data,
    SESOI_lower_function = bmbstats_SESOI_lower_function,
    SESOI_upper_function = bmbstats_SESOI_upper_function,
    estimator_function = bmbstats_estimator_function,
    control = control,
    na.rm = na.rm
  )
}


#' SESOI lower threshold for \code{\link{validity_analysis}}
#'
#' @inheritParams basic_arguments
#' @param criterion Character vector. Column name in \code{data} data frame
#' @param practical Character vector. Column name in \code{data} data frame
#'
#' @return SD of \code{criterion} column of the \code{data}
#'     data frame multiplied by -0.2 (Cohen's trivial)
#'
#' @export
#' @examples
#' data("agreement_data")
#' SESOI_lower_validity_func(
#'   data = agreement_data,
#'   criterion = "Criterion_score.trial1",
#'   practical = "Practical_score.trial1"
#' )
SESOI_lower_validity_func <- function(data,
                                      criterion,
                                      practical,
                                      na.rm = FALSE) {
  -stats::sd(data[[criterion]], na.rm = na.rm) * 0.2
}

#' SESOI upper threshold for \code{\link{validity_analysis}}
#'
#' @inheritParams basic_arguments
#' @param criterion Character vector. Column name in \code{data} data frame
#' @param practical Character vector. Column name in \code{data} data frame
#'
#' @return SD of \code{criterion} column of the \code{data}
#'     data frame multiplied by 0.2 (Cohen's trivial)
#'
#' @export
#' @examples
#' data("agreement_data")
#' SESOI_upper_validity_func(
#'   data = agreement_data,
#'   criterion = "Criterion_score.trial1",
#'   practical = "Practical_score.trial1"
#' )
SESOI_upper_validity_func <- function(data,
                                      criterion,
                                      practical,
                                      na.rm = FALSE) {
  stats::sd(data[[criterion]], na.rm = na.rm) * 0.2
}
