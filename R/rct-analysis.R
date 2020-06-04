
#' RCT Estimators
#'
#' \code{RCT_estimators} is used as a function for \code{\link{RCT_analysis}} function to provide
#'     estimators for RCT analysis
#'
#' @inheritParams basic_arguments
#' @param control_pre_test Numeric vector containing Control Pre-test observations
#' @param control_post_test Numeric vector containing Control Post-test observations
#' @param treatment_pre_test Numeric vector containing Treatment Pre-test observations
#' @param treatment_post_test Numeric vector containing Treatment Post-test observations
#'
#' @return Named numeric vector
#' @export
#'
#' @examples
#' set.seed(1666)
#'
#' RCT_estimators(
#'   control_pre_test = rnorm(20, 100, 10),
#'   control_post_test = rnorm(20, 105, 10),
#'   treatment_pre_test = rnorm(20, 100, 10),
#'   treatment_post_test = rnorm(20, 120, 10),
#'   SESOI_lower = -5,
#'   SESOI_upper = 5
#' )
RCT_estimators <- function(control_pre_test,
                           control_post_test,
                           treatment_pre_test,
                           treatment_post_test,
                           SESOI_lower = 0,
                           SESOI_upper = 0,
                           na.rm = FALSE) {
  SESOI_range <- SESOI_upper - SESOI_lower

  control_change <- control_post_test - control_pre_test
  treatment_change <- treatment_post_test - treatment_pre_test

  # SESOI summary
  SESOI_summary <- c(
    `SESOI lower` = SESOI_lower,
    `SESOI upper` = SESOI_upper,
    `SESOI range` = SESOI_range
  )

  # Group summaries
  group_summary <- c(
    `Control Group Pre-test mean` = mean(control_pre_test, na.rm = na.rm),
    `Control Group Pre-test SD` = stats::sd(control_pre_test, na.rm = na.rm),
    `Control Group Post-test mean` = mean(control_post_test, na.rm = na.rm),
    `Control Group Post-test SD` = stats::sd(control_post_test, na.rm = na.rm),

    `Treatment Group Pre-test mean` = mean(treatment_pre_test, na.rm = na.rm),
    `Treatment Group Pre-test SD` = stats::sd(treatment_pre_test, na.rm = na.rm),
    `Treatment Group Post-test mean` = mean(treatment_post_test, na.rm = na.rm),
    `Treatment Group Post-test SD` = stats::sd(treatment_post_test, na.rm = na.rm),

    `Pre-test pooled SD` = sd_pooled(treatment_pre_test, control_pre_test, na.rm = na.rm),
    `Pre-test Group difference` = mean(treatment_pre_test, na.rm = na.rm) - mean(control_pre_test, na.rm = na.rm),
    `Post-test Group difference` = mean(treatment_post_test, na.rm = na.rm) - mean(control_post_test, na.rm = na.rm)
  )

  # Change summary

  control_proportions <- mb_proportions(
    control_pre_test,
    control_post_test,
    paired = TRUE,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    na.rm = na.rm
  )

  treatment_proportions <- mb_proportions(
    treatment_pre_test,
    treatment_post_test,
    paired = TRUE,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    na.rm = na.rm
  )

  change_summary <- c(
    `Control Group Change mean` = mean(control_change, na.rm = na.rm),
    `Control Group Change SD` = stats::sd(control_change, na.rm = na.rm),
    `Control Group Cohen's d` = mean(control_change, na.rm = na.rm) /
      sd_pooled(treatment_pre_test, control_pre_test, na.rm = na.rm),
    `Control Group Change to SESOI` = mean(control_change, na.rm = na.rm) / SESOI_range,
    `Control Group Change SD to SESOI` = stats::sd(control_change, na.rm = na.rm) / SESOI_range,
    `Control Group pLower` = control_proportions$lower,
    `Control Group pEquivalent` = control_proportions$equivalent,
    `Control Group pHigher` = control_proportions$higher,

    # ----
    `Treatment Group Change mean` = mean(treatment_change, na.rm = na.rm),
    `Treatment Group Change SD` = stats::sd(treatment_change, na.rm = na.rm),
    `Treatment Group Cohen's d` = mean(treatment_change, na.rm = na.rm) /
      sd_pooled(treatment_pre_test, control_pre_test, na.rm = na.rm),
    `Treatment Group Change to SESOI` = mean(treatment_change, na.rm = na.rm) / SESOI_range,
    `Treatment Group Change SD to SESOI` = stats::sd(treatment_change, na.rm = na.rm) / SESOI_range,
    `Treatment Group pLower` = treatment_proportions$lower,
    `Treatment Group pEquivalent` = treatment_proportions$equivalent,
    `Treatment Group pHigher` = treatment_proportions$higher
  )

  # Treatment effects
  systematic_effect <- (mean(treatment_change, na.rm = na.rm) - mean(control_change, na.rm = na.rm))
  random_effect <- sqrt(stats::var(treatment_change, na.rm = na.rm) - stats::var(control_change, na.rm = na.rm))

  # For n observations use mean between treatment group and control group
  n_obs <- (length(treatment_change) + length(control_change)) / 2

  treatment_summary <- c(
    `Effect Cohen's d` = systematic_effect / stats::sd(control_change, na.rm = na.rm),
    `Systematic effect` = systematic_effect,
    `Random effect` = random_effect,
    `Systematic effect to SESOI` = systematic_effect / SESOI_range,
    `SESOI to Random effect` = SESOI_range / random_effect,

    # These use normal distribution
    # pLower = stats::pnorm(SESOI_lower, mean = systematic_effect, sd = random_effect),
    # pEquivalent = 1 - (stats::pnorm(SESOI_lower, mean = systematic_effect, sd = random_effect) +
    #   (1 - stats::pnorm(SESOI_upper, mean = systematic_effect, sd = random_effect))),
    # pHigher = 1 - stats::pnorm(SESOI_upper, mean = systematic_effect, sd = random_effect),

    # These use t-distribution
    pLower = stats::pt((SESOI_lower - systematic_effect) / random_effect, df = n_obs - 1),
    pEquivalent = 1 - (stats::pt((SESOI_lower - systematic_effect) /random_effect, df = n_obs - 1 ) +
                         (1 - stats::pt((SESOI_upper - systematic_effect) / random_effect, df = n_obs - 1))),
    pHigher = 1 - stats::pt((SESOI_upper - systematic_effect) / random_effect, df = n_obs - 1)
  )

  return(c(
    SESOI_summary,
    group_summary,
    change_summary,
    treatment_summary
  ))
}


#' RCT Estimators - Simple
#'
#' \code{RCT_estimators_simple} is used as a function for \code{\link{RCT_analysis}} function to provide a
#'     simple (or reduced) estimators for RCT analysis
#' @inheritParams RCT_estimators
#'
#' @return Named numeric vector
#' @export
#'
#' @examples
#' set.seed(1666)
#'
#' RCT_estimators_simple(
#'   control_pre_test = rnorm(20, 100, 10),
#'   control_post_test = rnorm(20, 105, 10),
#'   treatment_pre_test = rnorm(20, 100, 10),
#'   treatment_post_test = rnorm(20, 120, 10),
#'   SESOI_lower = -5,
#'   SESOI_upper = 5
#' )
RCT_estimators_simple <- function(control_pre_test,
                                  control_post_test,
                                  treatment_pre_test,
                                  treatment_post_test,
                                  SESOI_lower = 0,
                                  SESOI_upper = 0,
                                  na.rm = FALSE) {
  SESOI_range <- SESOI_upper - SESOI_lower

  control_change <- control_post_test - control_pre_test
  treatment_change <- treatment_post_test - treatment_pre_test

  # SESOI summary
  SESOI_summary <- c(
    `SESOI lower` = SESOI_lower,
    `SESOI upper` = SESOI_upper,
    `SESOI range` = SESOI_range
  )

  # Treatment effects
  systematic_effect <- (mean(treatment_change, na.rm = na.rm) - mean(control_change, na.rm = na.rm))
  random_effect <- sqrt(stats::var(treatment_change, na.rm = na.rm) - stats::var(control_change, na.rm = na.rm))

  # For n observations use mean between treatment group and control group
  n_obs <- (length(treatment_change) + length(control_change)) / 2

  treatment_summary <- c(
    `Systematic effect` = systematic_effect,
    `Random effect` = random_effect,
    `Systematic effect to SESOI` = systematic_effect / SESOI_range,
    `SESOI to Random effect` = SESOI_range / random_effect,

    # These use normal distribution
    # pLower = stats::pnorm(SESOI_lower, mean = systematic_effect, sd = random_effect),
    # pEquivalent = 1 - (stats::pnorm(SESOI_lower, mean = systematic_effect, sd = random_effect) +
    #   (1 - stats::pnorm(SESOI_upper, mean = systematic_effect, sd = random_effect))),
    # pHigher = 1 - stats::pnorm(SESOI_upper, mean = systematic_effect, sd = random_effect),

    # These use t-distribution
    pLower = stats::pt((SESOI_lower - systematic_effect) / random_effect, df = n_obs - 1),
    pEquivalent = 1 - (stats::pt((SESOI_lower - systematic_effect) /random_effect, df = n_obs - 1 ) +
                         (1 - stats::pt((SESOI_upper - systematic_effect) / random_effect, df = n_obs - 1))),
    pHigher = 1 - stats::pt((SESOI_upper - systematic_effect) / random_effect, df = n_obs - 1)
  )

  return(c(
    SESOI_summary,
    treatment_summary
  ))
}


#' RCT Analysis
#'
#' Wrapper function for \code{\link{bmbstats}} function to provide modular analysis for the Randomized Controlled Trials
#'
#' @param data Data frame
#' @param group Character vector indicating the name of the column in \code{data} where group information is stored
#' @param control_label Character vector indicating the label inside the \code{group} column for the control group
#' @param treatment_label Character vector indicating the label inside the \code{group} column for the treatment group
#' @param pre_test Character vector indicating the name of the column in \code{data} where Pre-test observations are located
#' @param post_test Character vector indicating the name of the column in \code{data} where Post-test observations are located
#' @param SESOI_lower Function or numeric scalar. Default is \code{\link{SESOI_lower_RCT_func}}
#' @param SESOI_upper Function or numeric scalar. Default is \code{\link{SESOI_upper_RCT_func}}
#' @param estimator_function Function for providing RCT estimators. Default is \code{\link{RCT_estimators}}
#' @param control Control object returned from \code{\link{model_control}} function.
#'     Use \code{boot_type}, \code{boot_samples}, \code{boot_strata}, and \code{confidence} to setup bootstrap.
#' @param na.rm Should NAs be removed? Default is \code{FALSE}
#'
#' @return Object of class `bmbstats_RCT_analysis`
#' @export
#'
#' @examples
#' set.seed(1666)
#'
#' data("vertical_jump_data")
#'
#' rct_model <- RCT_analysis(
#'   vertical_jump_data,
#'   group = "Group",
#'   treatment_label = "Treatment",
#'   control_label = "Control",
#'   pre_test = "Pre-test",
#'   post_test = "Post-test",
#'   control = model_control(boot_type = "perc", boot_samples = 1000)
#' )
#'
#' rct_model
#'
#' plot(rct_model)
RCT_analysis <- function(data,
                         group,
                         treatment_label,
                         control_label,
                         pre_test,
                         post_test,
                         SESOI_lower = SESOI_lower_RCT_func,
                         SESOI_upper = SESOI_upper_RCT_func,
                         estimator_function = RCT_estimators,
                         control = model_control(),
                         na.rm = FALSE) {

  # Filter out control and treatment data
  control_data <- data[data[, group] == control_label, ]
  treatment_data <- data[data[, group] == treatment_label, ]

  # filter out pre-test and post-test
  control_pre_test <- control_data[[pre_test]]
  control_post_test <- control_data[[post_test]]
  control_change <- control_post_test - control_pre_test
  control_id <- rownames(control_data)

  treatment_pre_test <- treatment_data[[pre_test]]
  treatment_post_test <- treatment_data[[post_test]]
  treatment_change <- treatment_post_test - treatment_pre_test
  treatment_id <- rownames(treatment_data)

  # create df
  rct_df <- rbind(
    data.frame(
      id = control_id,
      group = control_label,
      pre_test = control_pre_test,
      post_test = control_post_test,
      change = control_change
    ),
    data.frame(
      id =  treatment_id,
      group = treatment_label,
      pre_test = treatment_pre_test,
      post_test = treatment_post_test,
      change = treatment_change
    )
  )

  # ----------------------------------------------------
  # Wrapper functions
  bmbstats_SESOI_lower_function <- function(data, na.rm, init_boot) {
    control_pre_test <- data$pre_test[data$group == control_label]
    control_post_test <- data$post_test[data$group == control_label]

    treatment_pre_test <- data$pre_test[data$group == treatment_label]
    treatment_post_test <- data$post_test[data$group == treatment_label]

    func_num(
      SESOI_lower,
      control_pre_test = control_pre_test,
      control_post_test = control_post_test,
      treatment_pre_test = treatment_pre_test,
      treatment_post_test = treatment_post_test,
      na.rm = na.rm
    )
  }

  bmbstats_SESOI_upper_function <- function(data, na.rm, init_boot) {
    control_pre_test <- data$pre_test[data$group == control_label]
    control_post_test <- data$post_test[data$group == control_label]

    treatment_pre_test <- data$pre_test[data$group == treatment_label]
    treatment_post_test <- data$post_test[data$group == treatment_label]

    func_num(
      SESOI_upper,
      control_pre_test = control_pre_test,
      control_post_test = control_post_test,
      treatment_pre_test = treatment_pre_test,
      treatment_post_test = treatment_post_test,
      na.rm = na.rm
    )
  }

  bmbstats_estimator_function <- function(data, SESOI_lower, SESOI_upper, na.rm, init_boot) {
    control_pre_test <- data$pre_test[data$group == control_label]
    control_post_test <- data$post_test[data$group == control_label]

    treatment_pre_test <- data$pre_test[data$group == treatment_label]
    treatment_post_test <- data$post_test[data$group == treatment_label]

    estimators_list <- estimator_function(
      control_pre_test = control_pre_test,
      control_post_test = control_post_test,
      treatment_pre_test = treatment_pre_test,
      treatment_post_test = treatment_post_test,
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper,
      na.rm = na.rm
    )

    return(estimators_list)
  }

  # ---------------------------------------------
  # Call bmbstats
  results <- bmbstats(
    data = rct_df,
    SESOI_lower_function = bmbstats_SESOI_lower_function,
    SESOI_upper_function = bmbstats_SESOI_upper_function,
    estimator_function = bmbstats_estimator_function,
    control = control,
    na.rm = na.rm
  )
  class(results) <- "bmbstats_RCT_analysis"

  # --------------------------------------------
  # Response analysis for the treatment group
  treatment_responses <- rct_df
  treatment_responses <- treatment_responses[treatment_responses$group == treatment_label,]

  control_mean_change <- mean(control_change, na.rm = na.rm)
  control_SD_change <- stats::sd(control_change, na.rm = na.rm)
  n_observations <- length(control_change)

  # Smallest detectable change
  SDC <- control_SD_change * stats::qt(
    1 - ((1 - control$confidence) / 2),
    df = n_observations - 1
  )

  treatment_responses$SDC <- SDC
  treatment_responses$change_lower <- treatment_responses$change - SDC
  treatment_responses$change_upper <- treatment_responses$change + SDC
  treatment_responses$adjusted_change <- treatment_responses$change - control_mean_change
  treatment_responses$adjusted_change_lower <- treatment_responses$adjusted_change - SDC
  treatment_responses$adjusted_change_upper <- treatment_responses$adjusted_change + SDC

  # Save extra details
  # This is used for plotting
  results$extra <- list(
    data = rct_df,
    treatment_responses = treatment_responses,
    SESOI_lower = func_num(
      SESOI_lower,
      control_pre_test = control_pre_test,
      control_post_test = control_post_test,
      treatment_pre_test = treatment_pre_test,
      treatment_post_test = treatment_post_test,
      na.rm = na.rm
    ),
    SESOI_upper = func_num(
      SESOI_upper,
      control_pre_test = control_pre_test,
      control_post_test = control_post_test,
      treatment_pre_test = treatment_pre_test,
      treatment_post_test = treatment_post_test,
      na.rm = na.rm
    ),

    control_pre_test = control_pre_test,
    control_post_test = control_post_test,
    control_change = control_post_test - control_pre_test,
    treatment_pre_test = treatment_pre_test,
    treatment_post_test = treatment_post_test,
    treatment_change = treatment_post_test - treatment_pre_test,
    control_label = control_label,
    treatment_label = treatment_label,
    pre_test_label = pre_test,
    post_test_label = post_test
  )
  return(results)
}


#' SESOI lower threshold for RCT analysis
#'
#' \code{SESOI_lower_RCT_func} is used in \code{\link{RCT_analysis}} function
#'
#' @inheritParams RCT_estimators
#'
#' @return Pooled SD of \code{control_pre_test} and \code{treatment_pre_test}
#'     multiplied by 0.2 (Cohen's trivial)
#' @export
#'
#' @examples
#' set.seed(1666)
#'
#' SESOI_lower_RCT_func(
#'   control_pre_test = rnorm(20, 100, 10),
#'   control_post_test = rnorm(20, 105, 10),
#'   treatment_pre_test = rnorm(20, 100, 10),
#'   treatment_post_test = rnorm(20, 120, 10)
#' )
SESOI_lower_RCT_func <- function(control_pre_test,
                                 control_post_test,
                                 treatment_pre_test,
                                 treatment_post_test,
                                 na.rm = FALSE) {
  -sd_pooled(control_pre_test, treatment_pre_test, na.rm = na.rm) * 0.2
}

#' SESOI upper threshold for RCT analysis
#'
#' \code{SESOI_upper_RCT_func} is used in \code{\link{RCT_analysis}} function
#'
#' @inheritParams RCT_estimators
#'
#' @return Pooled SD of \code{control_pre_test} and \code{treatment_pre_test}
#'     multiplied by 0.2 (Cohen's trivial)
#' @export
#'
#' @examples
#' set.seed(1666)
#'
#' SESOI_upper_RCT_func(
#'   control_pre_test = rnorm(20, 100, 10),
#'   control_post_test = rnorm(20, 105, 10),
#'   treatment_pre_test = rnorm(20, 100, 10),
#'   treatment_post_test = rnorm(20, 120, 10)
#' )
SESOI_upper_RCT_func <- function(control_pre_test,
                                 control_post_test,
                                 treatment_pre_test,
                                 treatment_post_test,
                                 na.rm = FALSE) {
  sd_pooled(control_pre_test, treatment_pre_test, na.rm = na.rm) * 0.2
}
