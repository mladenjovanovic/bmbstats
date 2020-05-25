#' Loss functions
#' @inheritParams basic_arguments
#' @param observed Numeric vector
#' @param predicted Numeric vector
#' @param negative_weight How should negative residuals be weighted? Default is 1
#' @param positive_weight How should positive residuals be weighted? Default is 1
#' @param exponent Numeric scalar. Default is 1.
#' @name loss_functions
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
#' # Square loss
#' loss_quadratic(
#'   observed = observed,
#'   predicted = predicted,
#'   SESOI_lower = SESOI_lower,
#'   SESOI_upper = SESOI_upper
#' )
#'
#' # Absolute loss
#' loss_absolute(
#'   observed = observed,
#'   predicted = predicted,
#'   SESOI_lower = SESOI_lower,
#'   SESOI_upper = SESOI_upper
#' )
#'
#' # Difference
#' loss_difference(
#'   observed = observed,
#'   predicted = predicted,
#'   SESOI_lower = SESOI_lower,
#'   SESOI_upper = SESOI_upper
#' )
#'
#' # Huber loss
#' loss_huber(
#'   observed = observed,
#'   predicted = predicted,
#'   SESOI_lower = SESOI_lower,
#'   SESOI_upper = SESOI_upper
#' )
#'
#' # Hinge loss
#' loss_hinge(
#'   observed = observed,
#'   predicted = predicted,
#'   SESOI_lower = SESOI_lower,
#'   SESOI_upper = SESOI_upper
#' )
NULL


#' Quadratic loss
#' @rdname loss_functions
#' @export
loss_quadratic <- function(observed,
                           predicted,
                           SESOI_lower = 0,
                           SESOI_upper = 0,
                           negative_weight = 1,
                           positive_weight = 1,
                           na.rm = FALSE) {
  difference <- predicted - observed

  # Remove NAs
  if (na.rm) difference <- stats::na.omit(difference)

  # Add weight to positive or negative
  difference <- ifelse(
    difference < 0,
    difference * negative_weight,
    difference * positive_weight
  )

  return(difference^2)
}

#' Absolute loss
#' @rdname loss_functions
#' @export
loss_absolute <- function(observed,
                          predicted,
                          SESOI_lower = 0,
                          SESOI_upper = 0,
                          negative_weight = 1,
                          positive_weight = 1,
                          na.rm = FALSE) {
  difference <- predicted - observed

  # Remove NAs
  if (na.rm) difference <- stats::na.omit(difference)

  # Add weight to positive or negative
  difference <- ifelse(
    difference < 0,
    difference * negative_weight,
    difference * positive_weight
  )

  return(abs(difference))
}

#' Difference loss
#' @rdname loss_functions
#' @export
loss_difference <- function(observed,
                            predicted,
                            SESOI_lower = 0,
                            SESOI_upper = 0,
                            negative_weight = 1,
                            positive_weight = 1,
                            na.rm = FALSE) {
  difference <- predicted - observed

  # Remove NAs
  if (na.rm) difference <- stats::na.omit(difference)

  # Add weight to positive or negative
  difference <- ifelse(
    difference < 0,
    difference * negative_weight,
    difference * positive_weight
  )

  return(difference)
}

#' Huber loss
#' @rdname loss_functions
#' @export
loss_huber <- function(observed,
                       predicted,
                       SESOI_lower = 0,
                       SESOI_upper = 0,
                       negative_weight = 1,
                       positive_weight = 1,
                       na.rm = FALSE) {
  difference <- predicted - observed

  # Remove NAs
  if (na.rm) difference <- stats::na.omit(difference)

  ifelse(
    # Below SESOI_lower
    difference < SESOI_lower,
    abs(negative_weight * SESOI_lower * difference) - negative_weight * 0.5 * SESOI_lower^2,
    ifelse(
      # Between 0 and SESOI_lower
      difference < 0,
      negative_weight * 0.5 * difference^2,

      ifelse(
        # Between 0 and SESOI_upper
        difference < SESOI_upper,
        positive_weight * 0.5 * difference^2,
        # Over SESOI_upper
        abs(positive_weight * SESOI_upper * difference) - positive_weight * 0.5 * SESOI_upper^2
      )
    )
  )
}

#' Hinge loss
#' @rdname loss_functions
#' @export
loss_hinge <- function(observed,
                       predicted,
                       SESOI_lower = 0,
                       SESOI_upper = 0,
                       negative_weight = 1,
                       positive_weight = 1,
                       exponent = 1,
                       na.rm = FALSE) {
  difference <- predicted - observed

  # Remove NAs
  if (na.rm) difference <- stats::na.omit(difference)

  ifelse(
    # Below SESOI_lower
    difference < SESOI_lower,
    negative_weight * abs(difference - SESOI_lower)^exponent,

    ifelse(
      # Over SESOI_upper
      difference > SESOI_upper,
      positive_weight * abs(difference - SESOI_upper)^exponent,

      # Value for within SESOI
      0
    )
  )
}
