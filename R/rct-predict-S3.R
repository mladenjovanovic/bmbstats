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
#' @param type Type of plot.
#' @param ... Extra arguments. Use \code{\link{plot_control}} to control plotting style
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
    "residuals"))

  gg <- list(NULL)

  # Residuals
  if (type == "residuals") {
    gg <- RCT_predict_plot_residuals(x, ...)
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
