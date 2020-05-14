#' Fit a `cv_model`
#'
#' `cv_model()` fits a model.
#'
#' @param x Depending on the context:
#'
#'   * A __data frame__ of predictors.
#'   * A __matrix__ of predictors.
#'   * A __recipe__ specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' @param y When `x` is a __data frame__ or __matrix__, `y` is the outcome
#' specified as:
#'
#'   * A __data frame__ with 1 numeric column.
#'   * A __matrix__ with 1 numeric column.
#'   * A numeric __vector__.
#'
#' @param data When a __recipe__ or __formula__ is used, `data` is specified as:
#'
#'   * A __data frame__ containing both the predictors and the outcome.
#'
#' @param formula A formula specifying the outcome terms on the left-hand side,
#' and the predictor terms on the right-hand side.
#'
#' @param ... See Details
#' @details
#' Will be expanded
#'
#' @return
#'
#' A `cv_model` object.
#'
#' @examples
#' m1 <- cv_model(
#'   Sepal.Length~. -Species,
#'   iris
#' )
#' predict(m1, new_data = iris)
#' @export
cv_model <- function(x, ...) {
  UseMethod("cv_model")
}

#' @export
#' @rdname cv_model
cv_model.default <- function(x, ...) {
  stop("`cv_model()` is not defined for a '", class(x)[1], "'.", call. = FALSE)
}

# XY method - data frame

#' @export
#' @rdname cv_model
cv_model.data.frame <- function(x, y, ...) {
  processed <- hardhat::mold(x, y)
  cv_model_bridge(processed, ...)
}

# XY method - matrix

#' @export
#' @rdname cv_model
cv_model.matrix <- function(x, y, ...) {
  processed <- hardhat::mold(x, y)
  cv_model_bridge(processed, ...)
}

# Formula method

#' @export
#' @rdname cv_model
cv_model.formula <- function(formula, data, ...) {
  processed <- hardhat::mold(formula, data)
  cv_model_bridge(processed, ...)
}

# Recipe method

#' @export
#' @rdname cv_model
cv_model.recipe <- function(x, data, ...) {
  processed <- hardhat::mold(x, data)
  cv_model_bridge(processed, ...)
}

# ------------------------------------------------------------------------------
# Bridge

cv_model_bridge <- function(processed, ...) {
  predictors <- processed$predictors
  outcome <- processed$outcomes

  # Validate
  hardhat::validate_outcomes_are_univariate(outcome)
  outcome <- outcome[[1]]

  fit <- cv_model_impl(predictors, outcome, ...)

  new_cv_model(
    predictors =  fit$predictors,
    outcome = fit$outcome,
    model_func = fit$model_func,
    predict_func = fit$predict_func,
    perf_func = fit$perf_func,
    SESOI_lower = fit$SESOI_lower,
    SESOI_upper = fit$SESOI_upper,
    model = fit$model,
    predicted = fit$predicted,
    performance = fit$performance,
    control = fit$control,
    na.rm = fit$na.rm,
    blueprint = processed$blueprint
  )
}


# ------------------------------------------------------------------------------
# Implementation
cv_model_impl <- function(predictors,
                          outcome,
                          model_func = lm_model,
                          predict_func = generic_predict,
                          perf_func = performance_metrics,
                          SESOI_lower = SESOI_lower_func,
                          SESOI_upper = SESOI_upper_func,
                          control = model_control(),
                          na.rm = FALSE) {

  model <- model_func(
    predictors = predictors,
    outcome = outcome,
    SESOI_lower = func_num(SESOI_lower, predictors, outcome, na.rm),
    SESOI_upper = func_num(SESOI_upper, predictors, outcome, na.rm),
    na.rm = na.rm
  )

  predicted <- predict_func(
    model = model,
    predictors = predictors,
    SESOI_lower = func_num(SESOI_lower, predictors, outcome, na.rm),
    SESOI_upper = func_num(SESOI_upper, predictors, outcome, na.rm),
    na.rm = na.rm
  )

  performance <- perf_func(
    observed = outcome,
    predicted = predicted,
    SESOI_lower = func_num(SESOI_lower, predictors, outcome, na.rm),
    SESOI_upper = func_num(SESOI_upper, predictors, outcome, na.rm),
    na.rm = na.rm
  )

  list(
    predictors = predictors,
    outcome = outcome,
    model_func = model_func,
    predict_func = predict_func,
    perf_func = perf_func,
    SESOI_lower = SESOI_lower,
    SESOI_upper = SESOI_upper,
    model = model,
    predicted = predicted,
    performance = performance,
    control = control,
    na.rm = na.rm
  )
}

# ------------------------------------------------------------------------------
#' Simple linear regression model
#'
#' This model uses all \code{predictors} to model the \code{outcome} with
#'     \code{\link[stats]{lm}} function
#' @inheritParams basic_arguments
#' @returns \code{model} object
#' @export
#' @examples
#' lm_model(
#'   predictors = iris[2:3],
#'   outcome = iris[[1]]
#' )
lm_model <- function(predictors,
                     outcome,
                     SESOI_lower = 0,
                     SESOI_upper = 0,
                     na.rm = FALSE) {
  data <- cbind(.outcome = outcome, predictors)

  stats::lm(.outcome ~ ., data)
}


# ------------------------------------------------------------------------------
#' Predict using generic function
#'
#' Predicts using \code{\link[stats]{predict}} function and supplied \code{model}
#' @inheritParams basic_arguments
#' @export
#' @examples
#' m1 <- lm_model(
#'   predictors = iris[2:3],
#'   outcome = iris[[1]]
#' )
#'
#' generic_predict(m1)
generic_predict <- function(model,
                       predictors,
                       SESOI_lower = 0,
                       SESOI_upper = 0,
                       na.rm = FALSE) {
  stats::predict(model, newdata = predictors)
}

# ------------------------------------------------------------------------------
#' Performance metrics
#'
#' Returns a list of the most common performance metrics
#' @inheritParams basic_arguments
#' @export
#' @examples
#' m1 <- lm_model(
#'   predictors = iris[2:3],
#'   outcome = iris[[1]]
#' )
#'
#' predicted <- generic_predict(m1)
#'
#' performance_metrics(
#'   observed = iris[[1]],
#'   predicted = predicted
#' )
performance_metrics <- function(observed,
                               predicted,
                               SESOI_lower = 0,
                               SESOI_upper = 0,
                               na.rm = FALSE) {
  return(list(
    MBE = cost_MBE(
      observed = observed,
      predicted = predicted,
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper,
      na.rm = na.rm
    ),

    MAE = cost_MAE(
      observed = observed,
      predicted = predicted,
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper,
      na.rm = na.rm
    ),

    RMSE = cost_RMSE(
      observed = observed,
      predicted = predicted,
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper,
      na.rm = na.rm
    ),

    PPER = cost_PPER(
      observed = observed,
      predicted = predicted,
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper,
      na.rm = na.rm
    ),

    `SESOI to RMSE` = cost_SESOItoRMSE(
      observed = observed,
      predicted = predicted,
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper,
      na.rm = na.rm
    ),

    `R-squared` = cost_R_squared(
      observed = observed,
      predicted = predicted,
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper,
      na.rm = na.rm
    ),

    `MinErr` = cost_MinErr(
      observed = observed,
      predicted = predicted,
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper,
      na.rm = na.rm
    ),

    `MaxErr` = cost_MaxErr(
      observed = observed,
      predicted = predicted,
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper,
      na.rm = na.rm
    ),

    `MaxAbsErr` = cost_MaxAbsErr(
      observed = observed,
      predicted = predicted,
      SESOI_lower = SESOI_lower,
      SESOI_upper = SESOI_upper,
      na.rm = na.rm
    )
  ))
}

# ------------------------------------------------------------------------------
#' SESOI lower function
#'
#' Function that calculates lower SESOI threshold
#' @inheritParams basic_arguments
#' @export
#' @examples
#' SESOI_lower_func(outcome = iris[[1]])
SESOI_lower_func <- function(predictors, outcome, na.rm = FALSE) {
  -stats::sd(outcome, na.rm) * 0.2
}

# ------------------------------------------------------------------------------
#' SESOI upper function
#'
#' Function that calculates upper SESOI threshold
#' @inheritParams basic_arguments
#' @export
#' @examples
#' SESOI_upper_func(outcome = iris[[1]])
SESOI_upper_func <- function(predictors, outcome, na.rm = FALSE) {
  stats::sd(outcome, na.rm) * 0.2
}
