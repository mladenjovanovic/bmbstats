#' Reliability estimators
#'
#' \code{reliability_estimators} provides reliability estimators using the OLP regression model, where
#'     \code{trial1} is the outcome variable, and \code{trial2} is the predictor.
#'
#' @inheritParams basic_arguments
#' @param trial1 Character vector indicating column name in the \code{data}
#' @param trial2 Character vector indicating column name in the \code{data}
#'
#' @return Named vector with reliability estimators
#' @export
#'
#' @examples
#' data("agreement_data")
#'
#' reliability_estimators(
#'   data = agreement_data,
#'   trial1 = "Practical_score.trial1",
#'   trial2 = "Practical_score.trial2"
#' )
reliability_estimators <- function(data,
                                trial1,
                                trial2,
                                SESOI_lower = 0,
                                SESOI_upper = 0,
                                na.rm = FALSE) {

  # Check the length of the trial1 and trial2
  if(length(trial1) != 1) {
    stop("Trial1 must be a single column", call. = FALSE)
  }

  if(length(trial2) != 1) {
    stop("Trial2 must be a single column", call. = FALSE)
  }

  trial2_obs <- data[[trial2]]
  trial1_obs <- data[[trial1]]

  SESOI_range <- SESOI_upper - SESOI_lower

  olp_model <- OLP_regression(
    predictor =  trial2_obs,
    outcome = trial1_obs,
    na.rm = na.rm
  )

  pper <- PPER(
    sigma = olp_model$rse,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    df = length(trial1_obs) - 1
  )

  pearson_r <- stats::cor(trial2_obs, trial1_obs)
  r_squared <- pearson_r^2

  sdc <- olp_model$rse * stats::qt(1 - ((1 - 0.95)/2), df = length(trial1_obs) - 1)

  c(
    "SESOI lower" = SESOI_lower,
    "SESOI upper" = SESOI_upper,
    "SESOI range" = SESOI_range,
    "Intercept" = olp_model$intercept,
    "Slope" = olp_model$slope,
    "RSE" = olp_model$rse,
    "Pearson's r" = pearson_r,
    "R Squared" = r_squared,
    "SESOI to RSE" = SESOI_range / olp_model$rse,
    "PPER" = pper,
    "TE" = olp_model$rse / sqrt(2),
    "SDC" = sdc
  )

}


#' Reliability Analysis
#'
#' \code{reliability_analysis} represents a wrapper function for the \code{\link{bmbstats}} function.
#'     \code{reliability_analysis} runs the bootstrap reliability analysis of the \code{data} data frame using
#'     \code{estimator_function} to return the estimators
#' @param data Data frame
#' @param trial1 Character vector indicating column name in the \code{data}
#' @param trial2 Character vector indicating column name(s) in the \code{data}
#' @param SESOI_lower Function or numeric scalar. Default is \code{\link{SESOI_lower_reliability_func}}
#' @param SESOI_upper Function or numeric scalar. Default is \code{\link{SESOI_upper_reliability_func}}
#' @param estimator_function Function for providing reliability estimators. Default is \code{\link{reliability_estimators}}
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
#' val_analysis <- reliability_analysis(
#'   data = agreement_data,
#'   trial1 = "Practical_score.trial1",
#'   trial2 = "Practical_score.trial2",
#'   control = model_control(
#'     boot_type = "perc",
#'     boot_samples = 1000,
#'     seed = 1667)
#' )
#'
#' val_analysis
#'
#' plot(val_analysis)
reliability_analysis <- function(data,
                              trial1,
                              trial2,
                              SESOI_lower = SESOI_lower_reliability_func,
                              SESOI_upper = SESOI_upper_reliability_func,
                              estimator_function = reliability_estimators,
                              control = model_control(),
                              na.rm = FALSE) {

  # --------------------------------------------
  # Wrapper functions
  bmbstats_SESOI_lower_function <- function(data, na.rm, init_boot) {
    func_num(
      SESOI_lower,
      data = data,
      trial1 = trial1,
      trial2 = trial2,
      na.rm = na.rm
    )
  }

  bmbstats_SESOI_upper_function <- function(data, na.rm, init_boot) {
    func_num(
      SESOI_upper,
      data = data,
      trial1 = trial1,
      trial2 = trial2,
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
      trial1 = trial1,
      trial2 = trial2,
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper,
      na.rm = na.rm
    )

    return(estimators_list)
  }

  # --------------------------------------------
  # Call bmbstats
  results <- bmbstats(
    data = data,
    SESOI_lower_function = bmbstats_SESOI_lower_function,
    SESOI_upper_function = bmbstats_SESOI_upper_function,
    estimator_function = bmbstats_estimator_function,
    control = control,
    na.rm = na.rm
  )
}


#' SESOI lower threshold for \code{\link{reliability_analysis}}
#'
#' @inheritParams basic_arguments
#' @param trial1 Character vector. Column name in \code{data} data frame
#' @param trial2 Character vector. Column name in \code{data} data frame
#'
#' @return Pooled SD of \code{trial1} and \code{trial2} columns of the \code{data}
#'     data frame multiplied by -0.2 (Cohen's trivial)
#'
#' @export
#' @examples
#' data("agreement_data")
#' SESOI_lower_reliability_func(
#'   data = agreement_data,
#'   trial1 =  "Practical_score.trial1",
#'   trial2 = "Practical_score.trial2"
#' )
SESOI_lower_reliability_func <- function(data,
                                      trial1,
                                      trial2,
                                      na.rm = FALSE) {
  sd_pooled(data[[trial1]], data[[trial2]], na.rm = na.rm) * 0.2
}

#' SESOI upper threshold for \code{\link{reliability_analysis}}
#'
#' @inheritParams basic_arguments
#' @param trial1 Character vector. Column name in \code{data} data frame
#' @param trial2 Character vector. Column name in \code{data} data frame
#'
#' @return Pooled SD of \code{trial1} and \code{trial2} columns of the \code{data}
#'     data frame multiplied by 0.2 (Cohen's trivial)
#'
#' @export
#' @examples
#' data("agreement_data")
#' SESOI_lower_reliability_func(
#'   data = agreement_data,
#'   trial1 =  "Practical_score.trial1",
#'   trial2 = "Practical_score.trial2"
#' )
SESOI_upper_reliability_func <- function(data,
                                      trial1,
                                      trial2,
                                      na.rm = FALSE) {

  sd_pooled(data[[trial1]], data[[trial2]], na.rm = na.rm) * 0.2
}
