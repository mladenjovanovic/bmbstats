#' S3 method for plotting \code{\link{cv_model}} results
#' @param x Object of class \code{bmbstats_cv_model}
#' @param type Type of plot. Options are "residuals", "training-residuals",
#'     "testing-residuals", "bias-variance-index", "bias-variance-observed",
#'     "prediction-index", "prediction-observed", and "estimators".  Default is "residuals"
#' @param ... Extra arguments. Use \code{\link{plot_control}} to control plotting style.
#'
#' @export
#' @examples
#' data("vertical_jump_data")
#'
#' m1 <- cv_model(
#'   `Post-test` ~ `Pre-test` * Group * `Squat 1RM`,
#'   vertical_jump_data,
#'   control = model_control(
#'     cv_repeats = 10,
#'     cv_folds = 3,
#'     cv_strata = vertical_jump_data$Group
#'   )
#' )
#'
#' plot(m1, "residuals")
plot.bmbstats_cv_model <- function(x, type = "residuals", ...) {
  rlang::arg_match(type, c(
    "residuals",
    "training-residuals",
    "testing-residuals",
    "bias-variance-index",
    "bias-variance-observed",
    "prediction-index",
    "prediction-observed",
    "estimators"
  ))
  gg <- list(NULL)

  # Residuals plot
  if (type == "residuals") {
    gg <- plot_residuals(
      observed = x$outcome,
      predicted = x$predicted,
      SESOI_lower = func_num(x$SESOI_lower, x$predictors, x$outcome, x$na.rm),
      SESOI_upper = func_num(x$SESOI_upper, x$predictors, x$outcome, x$na.rm),
      ...
    )
  }

  # Residuals plot using all training cross-validated data
  if (type == "training-residuals") {
    gg <- plot_residuals(
      observed = x$cross_validation$data$training$outcome,
      predicted = x$cross_validation$data$training$predicted,
      SESOI_lower = func_num(
        x$SESOI_lower,
        x$predictors[x$cross_validation$data$training$index, ],
        x$cross_validation$data$training$outcome,
        x$na.rm
      ),
      SESOI_upper = func_num(
        x$SESOI_upper,
        x$predictors,
        x$cross_validation$data$training$outcome,
        x$na.rm
      ),
      ...
    )
  }

  # Residuals plot using all testing cross-validated data
  if (type == "testing-residuals") {
    gg <- plot_residuals(
      observed = x$cross_validation$data$testing$outcome,
      predicted = x$cross_validation$data$testing$predicted,
      SESOI_lower = func_num(
        x$SESOI_lower,
        x$predictors[x$cross_validation$data$training$index, ],
        x$cross_validation$data$training$outcome,
        x$na.rm
      ),
      SESOI_upper = func_num(
        x$SESOI_upper,
        x$predictors,
        x$cross_validation$data$training$outcome,
        x$na.rm
      ),
      ...
    )
  }

  # Bias-Variance against index
  if (type == "bias-variance-index") {
    gg <- plot_bias_variance_index(x, ...)
  }

  # Bias-Variance against observed
  if (type == "bias-variance-observed") {
    gg <- plot_bias_variance_observed(x, ...)
  }

  # Prediction residuals plot
  if (type == "prediction-index") {
    gg <- plot_prediction_index(x, ...)
  }

  # Prediction residuals plot
  if (type == "prediction-observed") {
    gg <- plot_prediction_observed(x, ...)
  }

  # Estimators
  if (type == "estimators") {
    gg <- plot_estimators(x, ...)
  }

  return(gg)
}

# -----------------------------------------
plot_prediction_index <- function(x, control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  index <- NULL
  residual <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  plot_data <- x$cross_validation$data$testing
  plot_data <- dplyr::group_by(plot_data, index)
  plot_data <- dplyr::mutate(
    plot_data,
    mean = mean(residual),
    min = min(residual),
    max = max(residual)
  )

  gg <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = index, group = index)
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
    ggplot2::geom_crossbar(
      ggplot2::aes(y = mean, ymin = min, ymax = max)
      # fill = "light grey"
    ) +
    ggplot2::ylab("Residual")

  gg
}

# -----------------------------------------
plot_prediction_observed <- function(x, control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  outcome <- NULL
  predicted <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++


  plot_data <- x$cross_validation$data$testing
  plot_data <- dplyr::group_by(plot_data, outcome)
  plot_data <- dplyr::mutate(
    plot_data,
    mean = mean(predicted),
    min = min(predicted),
    max = max(predicted)
  )

  gg <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = outcome, group = outcome)
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "grey", linetype = "dashed") +
    ggplot2::geom_crossbar(
      ggplot2::aes(y = mean, ymin = min, ymax = max)
      # fill = "light grey"
    ) +
    ggplot2::ylab("Predicted") +
    ggplot2::xlab("Observed")

  gg
}

# -----------------------------------------
plot_bias_variance_index <- function(x, control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  Error <- NULL
  index <- NULL
  value <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  plot_data <- tidyr::pivot_longer(
    x$cross_validation$bias_variance,
    cols = 3:5,
    names_to = "Error"
  )
  plot_data$Error <- factor(plot_data$Error, levels = c("MSE", "bias_squared", "variance"))

  gg <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = index,
      group = Error,
      color = Error,
      fill = Error
    )
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::geom_bar(ggplot2::aes(y = value), stat = "identity") +
    ggplot2::scale_color_manual(values = control$group_colors) +
    ggplot2::scale_fill_manual(values = control$group_colors) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab("index") +
    ggplot2::facet_wrap(~Error, ncol = 1) +
    ggplot2::theme(legend.position = "none")

  gg
}

# -----------------------------------------
plot_bias_variance_observed <- function(x, control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  Error <- NULL
  outcome <- NULL
  value <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++


  plot_data <- tidyr::pivot_longer(
    x$cross_validation$bias_variance,
    cols = 3:5,
    names_to = "Error"
  )
  plot_data$Error <- factor(plot_data$Error, levels = c("MSE", "bias_squared", "variance"))

  plot_data <- dplyr::group_by(plot_data, Error, outcome)
  plot_data <- dplyr::mutate(
    plot_data,
    mean = mean(value),
    min = min(value),
    max = max(value)
  )

  gg <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = outcome,
      group = Error,
      color = Error,
      fill = Error
    )
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = min, ymax = max), alpha = control$cloud_alpha, color = control$cloud_color) +
    ggplot2::geom_line(ggplot2::aes(y = mean)) +
    ggplot2::geom_line(ggplot2::aes(y = mean)) +
    ggplot2::scale_color_manual(values = control$group_colors) +
    ggplot2::scale_fill_manual(values = control$group_colors) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab("Observed") +
    ggplot2::facet_wrap(~Error, ncol = 1) +
    ggplot2::theme(legend.position = "none")

  gg
}

# -----------------------------------------
plot_residuals <- function(
                           observed,
                           predicted,
                           SESOI_lower = 0,
                           SESOI_upper = 0,
                           confidence = 0.95,
                           control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  residual <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++


  plot_data <- data.frame(
    observed = observed,
    predicted = predicted,
    residual = predicted - observed
  )

  n_observations <- nrow(plot_data)
  mean_difference <- mean(plot_data$residual)
  LOA <- stats::sd(plot_data$residual) * stats::qt(
    1 - ((1 - confidence) / 2),
    df = n_observations - 1
  )
  LOA_lower <- mean_difference - LOA
  LOA_upper <- mean_difference + LOA

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(y = residual, x = predicted)
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = SESOI_lower,
      ymax = SESOI_upper,
      fill = control$SESOI_color,
      alpha = control$SESOI_alpha
    ) +
    # Mean and LOA
    ggplot2::geom_hline(
      yintercept = mean_difference,
      color = "black",
      alpha = 0.4
    ) +
    ggplot2::geom_hline(
      yintercept = LOA_lower,
      color = "black",
      linetype = "dashed",
      alpha = 0.4
    ) +
    ggplot2::geom_hline(
      yintercept = LOA_upper,
      color = "black",
      linetype = "dashed",
      alpha = 0.4
    ) +
    ggplot2::geom_point(
      alpha = control$points_alpha,
      size = control$points_size,
      shape = control$points_shape,
      color = control$points_color,
      fill = control$points_fill
    ) +
    # Smooth
    ggplot2::geom_smooth(
      formula = y ~ x,
      method = control$smooth_method,
      level = confidence,
      se = control$smooth_se,
      size = control$smooth_size,
      alpha = control$smooth_alpha,
      fill = control$smooth_fill,
      color = control$smooth_color
    ) +
    ggplot2::labs(x = "Fitted", y = "Residuals")
}


# -----------------------------------------
plot_estimators <- function(x, control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  training <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++++++

  plot_data <- x$cross_validation$performance$summary$overall

  gg <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = 0
    )
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = training), color = "grey", linetype = "dashed") +
    ggplot2::geom_errorbar(
      ggplot2::aes(y = mean, ymin = min, ymax = max),
      width = 0.1
    ) +
    ggplot2::geom_point(ggplot2::aes(y = mean)) +
    ggplot2::facet_wrap(~metric, scales = "free_y") +
    ggplot2::xlim(-0.5, 0.5) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL) +
    ggplot2::theme(
      axis.line.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank()
    )

  gg
}

# =======================================================
#' S3 method for printing \code{\link{cv_model}} results
#' @param x Object of class \code{bmbstats_cv_model}
#' @param ... Extra arguments
#' @export
#' @examples
#' data("vertical_jump_data")
#'
#' m1 <- cv_model(
#'   `Post-test` ~ `Pre-test` * Group * `Squat 1RM`,
#'   vertical_jump_data,
#'   control = model_control(
#'     cv_repeats = 10,
#'     cv_folds = 3,
#'     cv_strata = vertical_jump_data$Group
#'   )
#' )
#'
#' m1
#' plot(m1, "residuals")
print.bmbstats_cv_model <- function(x, ...) {
  cat(
    "Training data consists of", ncol(x$predictors), ifelse(ncol(x$predictors) == 1, "predictor", "predictors"),
    "and", nrow(x$predictors), "observations."
  )


  if (is.list(x$cross_validation)) {
    cat(
      "\nCross-Validation of the model was performed using", x$control$cv_repeats,
      ifelse(x$control$cv_repeats == 1, "repeat", "repeats"), "of",
      x$control$cv_folds, "folds.\n"
    )

    cat("\nModel performance:\n\n")
    print(x$cross_validation$performance$summary$overall, row.names = FALSE)
  } else {
    cat("\nCross-Validation of the model was not performed.")
  }
}
