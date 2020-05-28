# =======================================================
#' S3 method for printing \code{\link{RCT_predict}} results
#' @param x Object of class \code{bmbstats_RCT_predict}
#' @param ... Extra arguments. Not used
#' @export
#' @examples
#' data("vertical_jump_data")
#'
#' m1 <- cv_model(
#'   `Post-test` ~ `Pre-test` + Group,
#'   vertical_jump_data,
#'   control = model_control(
#'     cv_repeats = 10,
#'     cv_folds = 3,
#'     cv_strata = vertical_jump_data$Group
#'   )
#' )
#'
#' m1_rct  <- RCT_predict(
#'   m1,
#'   new_data = vertical_jump_data,
#'   outcome = "Post-test",
#'   group = "Group",
#'   treatment_label = "Treatment",
#'   control_label = "Control"
#' )
#'
#' m1_rct
#' plot(m1_rct)
print.bmbstats_RCT_predict <- function(x, ...) {
  class(x) <- "bmbstats_cv_model"

  print(x)
}

#' S3 method for plotting \code{\link{RCT_predict}} results
#'
#'
#' @param x Object of class \code{bmbstats_RCT_predict}
#' @param type Type of plot. Options are "residuals", "prediction". Default is "residuals"
#' @param ... Extra arguments. Use \code{control} argument and \code{\link{plot_control}} function to control plotting style.
#'    \code{confidence} for setting confidence level, \code{metric} for selecting performance metric (default
#'    is "RMSE") and \code{metric_CV} for selecting source of CV metrics (defaults is "testing.pooled").
#' @export
#' @examples
#' data("vertical_jump_data")
#'
#' m1 <- cv_model(
#'   `Post-test` ~ `Pre-test` + Group,
#'   vertical_jump_data,
#'   control = model_control(
#'     cv_repeats = 10,
#'     cv_folds = 3,
#'     cv_strata = vertical_jump_data$Group
#'   )
#' )
#'
#' m1_rct  <- RCT_predict(
#'   m1,
#'   new_data = vertical_jump_data,
#'   outcome = "Post-test",
#'   group = "Group",
#'   treatment_label = "Treatment",
#'   control_label = "Control"
#' )
#'
#' m1_rct
#' plot(m1_rct)
plot.bmbstats_RCT_predict <- function(x, type = "residuals", ...) {
  rlang::arg_match(type, c(
    "residuals",
    "prediction"))

  gg <- list(NULL)

  # Residuals
  if (type == "residuals") {
    gg <- RCT_predict_plot_residuals(x, ...)
  }

  # Residuals
  if (type == "prediction") {
    gg <- RCT_predict_plot_prediction(x, ...)
  }

  return(gg)
}

# -------------------------------------------------------------------
RCT_predict_plot_residuals <- function(x, confidence = 0.95, control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  group <- NULL
  predicted <- NULL
  residual <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  plot_data <- x$extra$residual_df
  plot_data$group <- factor(plot_data$group)

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
    ggplot2::aes(y = residual, x = predicted, color = group, fill = group)
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = x$extra$SESOI_lower,
      ymax = x$extra$SESOI_upper,
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
      shape = control$points_shape
      #color = control$points_color,
      #fill = control$points_fill
    ) +
    # Smooth
    ggplot2::geom_smooth(
      ggplot2::aes(group = 1),
      formula = y ~ x,
      method = control$smooth_method,
      level = confidence,
      se = control$smooth_se,
      size = control$smooth_size,
      alpha = control$smooth_alpha,
      fill = control$smooth_fill,
      color = control$smooth_color
    ) +
    ggplot2::labs(x = "Fitted", y = "Residuals") +
    ggplot2::scale_color_manual(values = control$group_colors) +
    ggplot2::scale_fill_manual(values = control$group_colors) +
    ggplot2::theme(
      legend.position = control$legend_position)
}

# -------------------------------------------------------------------
RCT_predict_plot_prediction <- function(x, metric = "RMSE", metric_cv = "testing.pooled", control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  subject <- NULL
  predicted <- NULL
  observed <- NULL
  magnitude <- NULL
  predicted_upper <- NULL
  predicted_lower <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  plot_data <- x$extra$residual_df

  if (control$sort) {
    plot_data$subject <- factor(
      plot_data$subject,
      levels = plot_data$subject[order(plot_data$observed)])
  }

  # Get metrics
  df_metrics <- x$cross_validation$performance$summary$overall
  plot_data$prediction_metric <- df_metrics[df_metrics$metric == metric, metric_cv]

  plot_data$predicted_lower <- plot_data$predicted - plot_data$prediction_metric
  plot_data$predicted_upper <- plot_data$predicted + plot_data$prediction_metric

  ggplot2::ggplot(plot_data, ggplot2::aes(y = subject)) +
    cowplot::theme_cowplot(control$font_size) +
    ggstance::geom_linerangeh(ggplot2::aes(
      xmax = predicted,
      xmin = observed,
      color = magnitude
    ),
    size = control$bar_size, alpha = control$bar_alpha
    ) +
    ggstance::geom_linerangeh(ggplot2::aes(
      xmax = predicted_upper,
      xmin = predicted_lower
    ),
    size = control$summary_bar_size,
    color = control$summary_bar_color,
    alpha = control$summary_bar_alpha
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = predicted),
      shape = "|",
      size = control$points_size,
      alpha = control$points_alpha
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = observed),
      shape = control$points_shape,
      size = control$points_size,
      alpha = control$points_alpha) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(x$extra$outcome) +
    ggplot2::facet_wrap(group ~ ., scales = "free_y") +
    ggplot2::scale_discrete_manual(
      aesthetics = c("color", "fill"),
      values = control$effect_colors,
      drop = FALSE,
      limits = levels(plot_data$magnitude)
      ) +
    ggplot2::labs(color = "Residual") +
    ggplot2::theme(legend.position = control$legend_position)
}
