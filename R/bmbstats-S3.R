# =======================================================
#' S3 method for printing \code{\link{bmbstats}} results
#' @param x Object of class \code{bmbstats}
#' @param ... Extra arguments. Not used
#' @export
#' @examples
#' x <- bmbstats(iris,
#'   SESOI_lower_function = function(data, na.rm, init_boot) {
#'     sd(data$Sepal.Length) * -0.2
#'   },
#'   SESOI_upper_function = function(data, na.rm, init_boot) {
#'     sd(data$Sepal.Length) * 0.2
#'   },
#'   estimator_function = function(data, SESOI_lower, SESOI_upper, na.rm, init_boot) {
#'     list(mean = mean(data$Sepal.Length), SESOI_lower = SESOI_lower, SESOI_upper = SESOI_upper)
#'   },
#'   control = model_control(boot_type = "perc", boot_samples = 50)
#' )
#' x
print.bmbstats <- function(x, ...) {
  cat(
    paste0(
      "Bootstrap with ", x$control$boot_samples, " resamples and ", x$control$confidence * 100, "% ",
      x$control$boot_type, " confidence intervals.\n\n"
    )
  )

  print(x$estimators)
}

#' S3 method for plotting \code{\link{bmbstats}} results
#'
#' This function plots the bootstrap distribution
#'
#' @param x Object of class \code{bmbstats}
#' @param ... Extra arguments. Use \code{\link{plot_control}} to control plotting style
#' @export
#' @examples
#' x <- bmbstats(iris,
#'   SESOI_lower_function = function(data, na.rm, init_boot) {
#'     sd(data$Sepal.Length) * -0.2
#'   },
#'   SESOI_upper_function = function(data, na.rm, init_boot) {
#'     sd(data$Sepal.Length) * 0.2
#'   },
#'   estimator_function = function(data, SESOI_lower, SESOI_upper, na.rm, init_boot) {
#'     list(mean = mean(data$Sepal.Length), SESOI_lower = SESOI_lower, SESOI_upper = SESOI_upper)
#'   },
#'   control = model_control(boot_type = "perc", boot_samples = 50)
#' )
#' plot(x)
plot.bmbstats <- function(x, ...) {
  plot_bootstrap_distribution(x, ...)
}

# ---------------------------------------------------------------------------
plot_bootstrap_distribution <- function(bmbstats_obj,
                                        control = plot_control()) {


  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  value <- NULL
  lower <- NULL
  upper <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  boot_ci <- bmbstats_obj$estimators
  plot_data <- as.data.frame(bmbstats_obj$boot$t)

  colnames(plot_data) <- boot_ci$estimator
  plot_data <- tidyr::gather(plot_data, key = "estimator", "value")
  plot_data$estimator <- factor(
    plot_data$estimator,
    levels = levels(boot_ci$estimator)
  )

  # Create plot
  boot_plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = value)
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::geom_histogram(
      fill = control$group_colors[[1]],
      color = control$cloud_color,
      alpha = control$cloud_alpha,
      bins = control$bins
    ) +
    ggstance::geom_pointrangeh(
      data = boot_ci,
      ggplot2::aes(xmin = lower, xmax = upper, y = control$summary_bar_nudge),
      size = control$summary_bar_size,
      color = control$summary_bar_color
    ) +
    ggplot2::facet_wrap(~estimator, scales = "free") +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL) +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )

  return(boot_plot)
}
