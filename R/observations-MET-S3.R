# =======================================================
#' S3 method for printing \code{\link{observations_MET}} results
#' @param x Object of class \code{bmbstats_observations_MET}
#' @param ... Extra arguments. Not used
#' @export
#' @examples
#' data("bench_press_data")
#' obs_MET <- observations_MET(
#'   bench_press_data$Change,
#'   SESOI_lower = -5,
#'   SESOI_upper = 5,
#'   measurement_error = 2.5,
#'   alpha = 0.05
#' )
#'
#' obs_MET
#' plot(obs_MET)
print.bmbstats_observations_MET <- function(x, ...) {

  df <- do.call(data.frame, x)

  #rownames(df) <- df$observations_label

  print(df[-c(1, 3, 4, 5, 6, 12, 14, 15, 16, 17, 18)], row.names = FALSE)
}

# =======================================================
#' S3 method for plotting \code{\link{observations_MET}} results
#' @param x Object of class \code{bmbstats_observations_MET}
#' @param ... Extra arguments. Use \code{plot_control} for plotting options
#' @export
#' @examples
#' data("bench_press_data")
#' obs_MET <- observations_MET(
#'   bench_press_data$Change,
#'   SESOI_lower = -5,
#'   SESOI_upper = 5,
#'   measurement_error = 2.5,
#'   alpha = 0.05
#' )
#'
#' obs_MET
#' plot(obs_MET)
plot.bmbstats_observations_MET <- function(x, ...) {
  plot_bmbstats_observations_MET(x, ...)
}


# --------------------------------------------------------
plot_bmbstats_observations_MET <- function(x, control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  observations_label <- NULL
  observations <- NULL
  observations_lower <- NULL
  observations_upper <- NULL
  inference_label <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  x$inference_label <- paste0(" ", x$inference_label)

  plot_data <- do.call(data.frame, x)

  # Sort data
  if (control$sort) {
    plot_data$observations_label <- factor(
      plot_data$observations_label,
      levels = plot_data$observations_label[order(plot_data$observations)]
    )
  }

  ggplot2::ggplot(plot_data, ggplot2::aes(y = observations_label, x = observations)) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::annotate(
      "rect",
      xmin = x$SESOI_lower,
      xmax = x$SESOI_upper,
      ymin = -Inf,
      ymax = Inf,
      alpha = control$SESOI_alpha,
      fill = control$SESOI_color
    ) +
    #ggplot2::geom_vline(xintercept = 0, color = "dark grey") +
    ggstance::geom_pointrangeh(
      ggplot2::aes(xmin = observations_lower, xmax = observations_upper),
      size = control$summary_bar_size,
      color = control$summary_bar_color
    ) +
    ggplot2::geom_text(ggplot2::aes(
      y = observations_label,
      x = observations_upper,
      label = inference_label),
      hjust = "left") +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL) +
    ggplot2::theme(legend.position = "none")
}
