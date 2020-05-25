#' Cost functions
#' @inheritParams basic_arguments
#' @param observed Numeric vector
#' @param predicted Numeric vector
#' @param negative_weight How should negative residuals be weighted? Default is 1
#' @param positive_weight How should positive residuals be weighted? Default is 1
#' @name cost_functions
#' @examples
#' data("yoyo_mas_data")
#'
#' model <- lm(MAS ~ YoYoIR1, yoyo_mas_data)
#'
#' observed <- yoyo_mas_data$MAS
#' predicted <- predict(model)
#'
#' SESOI_lower <- -0.5
#' SESOI_upper <- 0.5
#'
#' # Mean Squared Error
#' cost_MSE(
#'   observed = observed,
#'   predicted = predicted,
#'   SESOI_lower = SESOI_lower,
#'   SESOI_upper = SESOI_upper
#' )
#'
#' # Mean Absolute Error
#' cost_MAE(
#'   observed = observed,
#'   predicted = predicted,
#'   SESOI_lower = SESOI_lower,
#'   SESOI_upper = SESOI_upper
#' )
#'
#' # Root Mean Squared Error
#' cost_RMSE(
#'   observed = observed,
#'   predicted = predicted,
#'   SESOI_lower = SESOI_lower,
#'   SESOI_upper = SESOI_upper
#' )
#'
#' # Bias
#' cost_MBE(
#'   observed = observed,
#'   predicted = predicted,
#'   SESOI_lower = SESOI_lower,
#'   SESOI_upper = SESOI_upper
#' )
#'
#' # Sum of Squared Errors
#' cost_SSE(
#'   observed = observed,
#'   predicted = predicted,
#'   SESOI_lower = SESOI_lower,
#'   SESOI_upper = SESOI_upper
#' )
#'
#' # Proportion of Practically Equivalent Residuals
#' cost_PPER(
#'   observed = observed,
#'   predicted = predicted,
#'   SESOI_lower = SESOI_lower,
#'   SESOI_upper = SESOI_upper
#' )
NULL

#' Mean Bias Error
#' @rdname cost_functions
#' @export
cost_MBE <- function(observed,
                predicted,
                SESOI_lower = 0,
                SESOI_upper = 0,
                negative_weight = 1,
                positive_weight = 1,
                na.rm = FALSE) {

 losses <- loss_difference(
   observed = observed,
   predicted = predicted,
   SESOI_lower = SESOI_lower,
   SESOI_upper = SESOI_upper,
   negative_weight = negative_weight,
   positive_weight = positive_weight,
   na.rm = na.rm
 )

 mean(losses, na.rm = na.rm)
 }

#' Mean Absolute Error
#' @rdname cost_functions
#' @export
cost_MAE <- function(observed,
                predicted,
                SESOI_lower = 0,
                SESOI_upper = 0,
                negative_weight = 1,
                positive_weight = 1,
                na.rm = FALSE) {

  losses <- loss_absolute(
    observed = observed,
    predicted = predicted,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    negative_weight = negative_weight,
    positive_weight = positive_weight,
    na.rm = na.rm
  )

  mean(losses, na.rm = na.rm)
}


#' Mean Squared Error
#' @rdname cost_functions
#' @export
cost_MSE <- function(observed,
                predicted,
                SESOI_lower = 0,
                SESOI_upper = 0,
                negative_weight = 1,
                positive_weight = 1,
                na.rm = FALSE) {

  losses <- loss_quadratic(
    observed = observed,
    predicted = predicted,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    negative_weight = negative_weight,
    positive_weight = positive_weight,
    na.rm = na.rm
  )

  mean(losses, na.rm = na.rm)
}

#' Root Mean Squared Error
#' @rdname cost_functions
#' @export
cost_RMSE <- function(observed,
                predicted,
                SESOI_lower = 0,
                SESOI_upper = 0,
                negative_weight = 1,
                positive_weight = 1,
                na.rm = FALSE) {

  losses <- loss_quadratic(
    observed = observed,
    predicted = predicted,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    negative_weight = negative_weight,
    positive_weight = positive_weight,
    na.rm = na.rm
  )

  sqrt(mean(losses, na.rm = na.rm))
}

#' Sum of Squared Error
#' @rdname cost_functions
#' @export
cost_SSE <- function(observed,
                 predicted,
                 SESOI_lower = 0,
                 SESOI_upper = 0,
                 negative_weight = 1,
                 positive_weight = 1,
                 na.rm = FALSE) {

  losses <- loss_quadratic(
    observed = observed,
    predicted = predicted,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    negative_weight = negative_weight,
    positive_weight = positive_weight,
    na.rm = na.rm
  )

  sum(losses, na.rm = na.rm)
}

#' Minimum Error
#' @rdname cost_functions
#' @export
cost_MinErr <- function(observed,
                predicted,
                SESOI_lower = 0,
                SESOI_upper = 0,
                negative_weight = 1,
                positive_weight = 1,
                na.rm = FALSE) {

  losses <- loss_difference(
    observed = observed,
    predicted = predicted,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    negative_weight = negative_weight,
    positive_weight = positive_weight,
    na.rm = na.rm
  )

  min(losses, na.rm = na.rm)
}

#' Maximum Error
#' @rdname cost_functions
#' @export
cost_MaxErr <- function(observed,
                   predicted,
                   SESOI_lower = 0,
                   SESOI_upper = 0,
                   negative_weight = 1,
                   positive_weight = 1,
                   na.rm = FALSE) {

  losses <- loss_difference(
    observed = observed,
    predicted = predicted,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    negative_weight = negative_weight,
    positive_weight = positive_weight,
    na.rm = na.rm
  )

  max(losses, na.rm = na.rm)
}

#' Maximum Absolute Error
#' @rdname cost_functions
#' @export
cost_MaxAbsErr <- function(observed,
                   predicted,
                   SESOI_lower = 0,
                   SESOI_upper = 0,
                   negative_weight = 1,
                   positive_weight = 1,
                   na.rm = FALSE) {

  losses <- loss_absolute(
    observed = observed,
    predicted = predicted,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    negative_weight = negative_weight,
    positive_weight = positive_weight,
    na.rm = na.rm
  )

  max(losses, na.rm = na.rm)
}

#' Maximum Squared Error
#' @rdname cost_functions
#' @export
cost_MaxSqErr <- function(observed,
                     predicted,
                     SESOI_lower = 0,
                     SESOI_upper = 0,
                     negative_weight = 1,
                     positive_weight = 1,
                     na.rm = FALSE) {

  losses <- loss_quadratic(
    observed = observed,
    predicted = predicted,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    negative_weight = negative_weight,
    positive_weight = positive_weight,
    na.rm = na.rm
  )

  max(losses, na.rm = na.rm)
}

#' R squared(Variance explained)
#' @rdname cost_functions
#' @export
cost_R_squared <- function(observed,
                     predicted,
                     SESOI_lower = 0,
                     SESOI_upper = 0,
                     negative_weight = 1,
                     positive_weight = 1,
                     na.rm = FALSE) {

  total_MSE <- stats::var(observed, na.rm = na.rm)
  predicted_MSE <- stats::var(predicted - observed, na.rm = na.rm)

  (total_MSE - predicted_MSE) / total_MSE
}

#' SESOI to RMSE
#' @rdname cost_functions
#' @export
cost_SESOItoRMSE <- function(observed,
                     predicted,
                     SESOI_lower = 0,
                     SESOI_upper = 0,
                     negative_weight = 1,
                     positive_weight = 1,
                     na.rm = FALSE) {

  rmse <- cost_RMSE(
    observed = observed,
    predicted = predicted,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    negative_weight = negative_weight,
    positive_weight = positive_weight,
    na.rm = na.rm
  )

  (SESOI_upper - SESOI_lower) / rmse
}

#' Proportion of practically equivalent residuals
#'
#' This method uses algebraic method assuming normal distribution of the residuals.
#'     This is done by using \code{\link[stats]{sd}} rather than RSE from
#'     \code{\link[stats]{lm}} model.
#' @rdname cost_functions
#' @export
cost_PPER <- function(observed,
                        predicted,
                        SESOI_lower = 0,
                        SESOI_upper = 0,
                        negative_weight = 1,
                        positive_weight = 1,
                        na.rm = FALSE) {

  difference <- predicted - observed

  # Remove NAs
  if(na.rm) difference <- stats::na.omit(difference)

  mean_diff <- mean(difference)
  sd_diff <- stats::sd(difference)

  stats::pnorm(SESOI_upper, mean_diff, sd_diff) -
    stats::pnorm(SESOI_lower, mean_diff, sd_diff)
}

#' Mean Huber Error
#' @rdname cost_functions
#' @export
cost_MHE <- function(observed,
                predicted,
                SESOI_lower = 0,
                SESOI_upper = 0,
                negative_weight = 1,
                positive_weight = 1,
                na.rm = FALSE) {

  losses <- loss_huber(
    observed = observed,
    predicted = predicted,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    negative_weight = negative_weight,
    positive_weight = positive_weight,
    na.rm = na.rm
  )

  mean(losses, na.rm = na.rm)
}

#' Root Mean Huber Error
#' @rdname cost_functions
#' @export
cost_RMHE <- function(observed,
                predicted,
                SESOI_lower = 0,
                SESOI_upper = 0,
                negative_weight = 1,
                positive_weight = 1,
                na.rm = FALSE) {

  losses <- loss_huber(
    observed = observed,
    predicted = predicted,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    negative_weight = negative_weight,
    positive_weight = positive_weight,
    na.rm = na.rm
  )

  sqrt(mean(losses, na.rm = na.rm))
}
