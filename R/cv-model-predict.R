#' Predict from a `cv_model`
#'
#' @param object A `cv_model` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param type A single character. The type of predictions to generate.
#' Valid options are:
#'
#' - `"numeric"` for numeric predictions.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @examples
#' m1 <- cv_model(
#'   Sepal.Length ~ . - Species,
#'   iris
#' )
#' predict(m1, new_data = iris)
#' @export
predict.bmbstats_cv_model <- function(object, new_data = NULL, type = "numeric", ...) {
  forged <- hardhat::forge(new_data, object$blueprint)
  rlang::arg_match(type, valid_predict_types())
  predict_cv_model_bridge(type, object, forged$predictors)
}

valid_predict_types <- function() {
  c("numeric")
}

# ------------------------------------------------------------------------------
# Bridge

predict_cv_model_bridge <- function(type, model, predictors) {
  predict_function <- get_predict_function(type)
  predictions <- predict_function(model, predictors)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}

get_predict_function <- function(type) {
  switch(
    type,
    numeric = predict_cv_model_numeric
  )
}

# ------------------------------------------------------------------------------
# Implementation

predict_cv_model_numeric <- function(model, predictors) {

  predictions <- model$predict_func(
    model = model$model,
    predictors = predictors,
    SESOI_lower = func_num(model$SESOI_lower, predictors, model$outcome, model$na.rm),
    SESOI_upper = func_num(model$SESOI_upper, predictors, model$outcome, model$na.rm),
    na.rm = model$na.rm
  )
  # hardhat::spruce_numeric(predictions)
}
