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
#' Extra parameters using \code{...} are forwarded to \code{\link{bmbstats_impl}}
#'    implementation function. These parameters are the following:
#' \describe{
#'   \item{model_func}{Model function. Default is \code{\link{lm_model}}. See also
#'   \code{\link{baseline_model}}}
#'   \item{predict_func}{Predict function. Default is \code{\link{generic_predict}}}
#'   \item{perf_func}{Model performance function. Default is \code{\link{performance_metrics}}}
#'   \item{SESOI_lower}{Function or numeric scalar. Default is \code{\link{SESOI_lower_func}}}
#'   \item{SESOI_upper}{Function or numeric scalar. Default is \code{\link{SESOI_upper_func}}}
#'   \item{control}{Control structure using \code{\link{model_control}}. The parameters
#'   used in \code{cv_model} are \code{cv_folds}, and \code{cv_strata}}
#'   \item{na.rm}{Should NAs be removed? Default is FALSE. This is forwarded  to
#'   \code{model_func}, \code{predict_func}, \code{perfr_func}, \code{SESOI_lower},
#'   and \code{SESOI_upper}}
#' }
#'
#' In summary, \code{cv_model} represents a wrapper function, that performs \code{model_func} within
#'  the cross-validation loop and provide it's predictive performance metrics using \code{perf_func}
#'
#' @return
#'
#' A `bmbstats_cv_model` object.
#'
#' @examples
#' m1 <- cv_model(
#'   Sepal.Length ~ . - Species,
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
    predictors = fit$predictors,
    outcome = fit$outcome,
    model_func = fit$model_func,
    predict_func = fit$predict_func,
    perf_func = fit$perf_func,
    SESOI_lower = fit$SESOI_lower,
    SESOI_upper = fit$SESOI_upper,
    model = fit$model,
    predicted = fit$predicted,
    performance = fit$performance,
    residual = fit$residual,
    residual_magnitude = fit$residual_magnitude,
    cross_validation = fit$cross_validation,
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

  # Set-up seed for reproducibility
  set.seed(control$seed)

  # ------------------------------------
  # Training models
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

  residual <- predicted - outcome
  residual_magnitude <- get_magnitude(
    residual,
    SESOI_lower = func_num(SESOI_lower, predictors, outcome, na.rm),
    SESOI_upper = func_num(SESOI_upper, predictors, outcome, na.rm),
  )

  performance <- perf_func(
    observed = outcome,
    predicted = predicted,
    SESOI_lower = func_num(SESOI_lower, predictors, outcome, na.rm),
    SESOI_upper = func_num(SESOI_upper, predictors, outcome, na.rm),
    na.rm = na.rm
  )

  # ------------------------------------
  # Cross-validation
  iter <- control$iter
  cv_folds <- control$cv_folds
  cv_repeats <- control$cv_repeats
  cv_strata <- control$cv_strata

  if (!is.null(cv_folds)) {
    # If there is no repeats defined then assume 1
    if (is.null(cv_repeats)) {
      cv_repeats <- 1
      control$cv_repeats <- 1
    }
  }

  # Check if strata is NULL
  if (is.null(cv_strata)) {
    cv_strata <- seq(1, nrow(predictors))
    control$cv_strata <- cv_strata
  }

  # Progress bar
  # Show progress bar
  if (iter) {
    pb <- progress::progress_bar$new(
      total = cv_folds * cv_repeats,
      format = "(:spin) [:bar] :percent eta: :eta"
    )
    pb$tick(0)
    message(
      paste("Cross-validating: ",
        cv_folds,
        " folds, ",
        cv_repeats,
        " repeats",
        sep = ""
      )
    )
  }

  # Create CV folds
  # Set-up seed for reproducibility
  set.seed(control$seed)

  cv_folds <- caret::createMultiFolds(
    y = cv_strata,
    k = cv_folds,
    times = cv_repeats
  )

  n_observations <- nrow(predictors)
  # Convert fold lists into TRUE/FALSE testing index for every resample
  cv_index <- t(sapply(cv_folds, function(x) {
    train_observations <- rep(FALSE, n_observations)
    train_observations[x] <- TRUE
    return(train_observations)
  }))

  # ----------------------------------------------------------------------------
  # Loop through CV folds
  cv_results <- purrr::map2(cv_folds, names(cv_folds), function(obs, fold_name) {
    if (iter) pb$tick()

    # Split the data into train and test
    train_predictors <- predictors[obs, ]
    test_predictors <- predictors[-obs, ]
    train_outcome <- outcome[obs]
    test_outcome <- outcome[-obs]

    train_index <- obs
    test_index <- seq(1, nrow(predictors))[-obs]

    # Set-up seed for reproducibility
    set.seed(control$seed)

    # Train model using training data
    train_model <- model_func(
      predictors = train_predictors,
      outcome = train_outcome,
      SESOI_lower = func_num(SESOI_lower, train_predictors, train_outcome, na.rm),
      SESOI_upper = func_num(SESOI_upper, train_predictors, train_outcome, na.rm),
      na.rm = na.rm
    )

    train_predicted <- predict_func(
      model = train_model,
      predictors = train_predictors,
      SESOI_lower = func_num(SESOI_lower, train_predictors, train_outcome, na.rm),
      SESOI_upper = func_num(SESOI_upper, train_predictors, train_outcome, na.rm),
      na.rm = na.rm
    )

    train_residual <- train_predicted - train_outcome

    train_residual_magnitude <- get_magnitude(
      train_residual,
      SESOI_lower = func_num(SESOI_lower, train_predictors, train_outcome, na.rm),
      SESOI_upper = func_num(SESOI_upper, train_predictors, train_outcome, na.rm),
    )

    train_performance <- perf_func(
      observed = train_outcome,
      predicted = train_predicted,
      SESOI_lower = func_num(SESOI_lower, train_predictors, train_outcome, na.rm),
      SESOI_upper = func_num(SESOI_upper, train_predictors, train_outcome, na.rm),
      na.rm = na.rm
    )

    # Test model
    test_predicted <- predict_func(
      model = train_model,
      predictors = test_predictors,
      SESOI_lower = func_num(SESOI_lower, train_predictors, train_outcome, na.rm),
      SESOI_upper = func_num(SESOI_upper, train_predictors, train_outcome, na.rm),
      na.rm = na.rm
    )

    test_residual <- test_predicted - test_outcome

    test_residual_magnitude <- get_magnitude(
      test_residual,
      SESOI_lower = func_num(SESOI_lower, train_predictors, train_outcome, na.rm),
      SESOI_upper = func_num(SESOI_upper, train_predictors, train_outcome, na.rm),
    )

    test_performance <- perf_func(
      observed = test_outcome,
      predicted = test_predicted,
      SESOI_lower = func_num(SESOI_lower, train_predictors, train_outcome, na.rm),
      SESOI_upper = func_num(SESOI_upper, train_predictors, train_outcome, na.rm),
      na.rm = na.rm
    )

    # Save everything in a list
    list(
      training = list(
        predictors = train_predictors,
        outcome = train_outcome,
        index = train_index,
        predicted = train_predicted,
        residual = train_residual,
        residual_magnitude = train_residual_magnitude,
        performance = train_performance
        # Don't save the model
        # model = model
      ),
      testing = list(
        predictors = test_predictors,
        outcome = test_outcome,
        index = test_index,
        predicted = test_predicted,
        residual = test_residual,
        residual_magnitude = test_residual_magnitude,
        performance = test_performance
      )
    )
  })

  # ------------------------------------------
  # Create CV summaries

  # Pooled training data
  training_data <- purrr::map2_df(cv_results, names(cv_results), function(cv_folds, fold_name) {
    data.frame(
      fold = fold_name,
      index = cv_folds$training$index,
      outcome = cv_folds$training$outcome,
      predicted = cv_folds$training$predicted,
      residual = cv_folds$training$residual,
      residual_magnitude = cv_folds$training$residual_magnitude,
      stringsAsFactors = FALSE
    )
  })

  # Pooled training data performance
  training_performance <- perf_func(
    observed = training_data$outcome,
    predicted = training_data$predicted,
    SESOI_lower = func_num(SESOI_lower, predictors[training_data$index, ], training_data$outcome, na.rm),
    SESOI_upper = func_num(SESOI_upper, predictors[training_data$index, ], training_data$outcome, na.rm),
    na.rm = na.rm
  )

  # Pooled testing data
  testing_data <- purrr::map2_df(cv_results, names(cv_results), function(cv_folds, fold_name) {
    data.frame(
      fold = fold_name,
      index = cv_folds$testing$index,
      outcome = cv_folds$testing$outcome,
      predicted = cv_folds$testing$predicted,
      residual = cv_folds$testing$residual,
      residual_magnitude = cv_folds$testing$residual_magnitude,
      stringsAsFactors = FALSE
    )
  })

  # Pooled testing data performance
  testing_performance <- perf_func(
    observed = testing_data$outcome,
    predicted = testing_data$predicted,
    SESOI_lower = func_num(SESOI_lower, predictors[training_data$index, ], training_data$outcome, na.rm),
    SESOI_upper = func_num(SESOI_upper, predictors[training_data$index, ], training_data$outcome, na.rm),
    na.rm = na.rm
  )

  # Training performance across folds
  cv_training_performance <- purrr::map2_df(cv_results, names(cv_results), function(cv_folds, fold_name) {
    data.frame(
      fold = fold_name,
      metric = names(cv_folds$training$performance),
      value = cv_folds$training$performance,
      stringsAsFactors = FALSE
    )
  })

  cv_training_performance$metric <- factor(
    cv_training_performance$metric,
    levels = names(performance)
  )

  cv_training_performance_summary <- split(cv_training_performance, cv_training_performance$metric)
  cv_training_performance_summary <- purrr::map_df(cv_training_performance_summary, function(metric) {
    data.frame(
      metric = metric$metric[[1]],
      mean = mean(metric$value),
      SD = stats::sd(metric$value),
      min = min(metric$value),
      max = max(metric$value)
    )
  })
  cv_training_performance_summary$metric <- factor(
    cv_training_performance_summary$metric,
    levels = names(performance)
  )

  # Testing performance across folds
  cv_testing_performance <- purrr::map2_df(cv_results, names(cv_results), function(cv_folds, fold_name) {
    data.frame(
      fold = fold_name,
      metric = names(cv_folds$testing$performance),
      value = cv_folds$testing$performance,
      stringsAsFactors = FALSE
    )
  })

  cv_testing_performance$metric <- factor(
    cv_testing_performance$metric,
    levels = names(performance)
  )

  cv_testing_performance_summary <- split(cv_testing_performance, cv_testing_performance$metric)
  cv_testing_performance_summary <- purrr::map_df(cv_testing_performance_summary, function(metric) {
    data.frame(
      metric = metric$metric[[1]],
      mean = mean(metric$value),
      SD = stats::sd(metric$value),
      min = min(metric$value),
      max = max(metric$value)
    )
  })
  cv_testing_performance_summary$metric <- factor(
    cv_testing_performance_summary$metric,
    levels = names(performance)
  )

  cv_overall_performance_summary <- data.frame(
    metric = names(performance),
    training = performance,
    training.pooled = training_performance,
    testing.pooled = testing_performance,
    cv_testing_performance_summary[-1],
    row.names = NULL
  )

  # Bias-Variance
  bias_variance <- split(testing_data, testing_data$index)
  bias_variance <- purrr::map_df(bias_variance, function(x) {
    index <- x$index[[1]]
    outcome <- x$outcome[[1]]
    MSE <- mean(x$residual^2)
    bias_squared <- (mean(x$predicted) - outcome)^2
    variance <- mean((mean(x$predicted) - x$predicted)^2)

    data.frame(
      index = index,
      outcome = outcome,
      MSE = MSE,
      bias_squared = bias_squared,
      variance = variance
    )
  })

  # Save CV results
  cross_validation <- list(
    cv_folds = control$cv_folds,
    cv_repeat = control$cv_repeats,
    cv_strata = control$cv_strata,
    cv_index = cv_index,
    data = list(
      training = training_data,
      testing = testing_data
    ),
    performance = list(
      training = training_performance,
      testing = testing_performance,
      folds = list(
        training = cv_training_performance,
        testing = cv_testing_performance
      ),
      summary = list(
        training = cv_training_performance_summary,
        testing = cv_testing_performance_summary,
        overall = cv_overall_performance_summary
      )
    ),
    bias_variance = bias_variance,
    folds = cv_results
  )
  # ------------------------------------
  # Save results in the object

  if (iter) message("Done!")

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
    residual = residual,
    residual_magnitude = residual_magnitude,
    cross_validation = cross_validation,
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
#' Baseline model
#'
#' This model return the mean of the \code{outcome} as a prediction
#' @inheritParams basic_arguments
#' @returns \code{model} object
#' @export
#' @examples
#' baseline_model(
#'   predictors = iris[2:3],
#'   outcome = iris[[1]]
#' )
baseline_model <- function(predictors,
                           outcome,
                           SESOI_lower = 0,
                           SESOI_upper = 0,
                           na.rm = FALSE) {
  data <- cbind(.outcome = outcome, predictors)

  stats::lm(.outcome ~ 1, data)
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
#' Returns named vector of the most common performance metrics
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
  c(
    # SESOI_lower = SESOI_lower,
    # SESOI_upper = SESOI_upper,
    # SESOI_range = SESOI_upper - SESOI_lower,
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
  )
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
