#' S3 method for plotting \code{\link{cv_model}} results
#' @param x Object of class \code{bmbstats_cv_model}
#' @param type Type of plot. Options are "residuals",  being default
#' @param ... Extra arguments
#' @export
#' @examples
#' m1 <- cv_model(
#'   Sepal.Length~. -Species,
#'   iris
#' )
#' plot(m1, "residuals")
plot.bmbstats_cv_model <- function(x, type = "residuals", ...) {
  rlang::arg_match(type, c("residuals"))
  gg <- list(NULL)

  # Residuals plot
  if (type == "residuals") {
   gg <- plot_residuals(
     observed = x$outcome,
     predicted = x$predicted,
     SESOI_lower = func_num(x$SESOI_lower, x$predictors, x$outcome, x$na.rm),
     SESOI_upper = func_num(x$SESOI_upper, x$predictors, x$outcome, x$na.rm),
     ...)
  }

  return(gg)
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

#' S3 method for printing \code{\link{cv_model}} results
#' @param x Object of class \code{bmbstats_cv_model}
#' @param ... Extra arguments
#' @export
#' @examples
#' m1 <- cv_model(
#'   Sepal.Length~. -Species,
#'   iris
#' )
#' m1
print.bmbstats_cv_model <- function(x, ...) {
  cat("sdfsfsdf")
}
