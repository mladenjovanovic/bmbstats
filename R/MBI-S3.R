# =======================================================
#' S3 method for printing \code{\link{bootstrap_MBI}} results
#' @param x Object of class \code{bmbstats_MBI}
#' @param ... Extra arguments. Not used
#' @export
#' @examples
#' mean_MBI <- bootstrap_MBI(
#'   describe_data(rnorm(10, 100, 20)),
#'   estimator = "mean",
#'   SESOI_lower = 95,
#'   SESOI_upper = 105
#' )
#' mean_MBI
#' plot(mean_MBI, control = plot_control(points_jitter = FALSE))
print.bmbstats_MBI <- function(x, ...) {

  cat(paste0("Magnitude-based inference for the `", x$estimator$name,"` estimator\n"))
  cat(paste0("Bootstrap result: ", x$estimator$name, "=", round(x$estimator$value, 3), ", ",
             round(x$estimator$confidence * 100, 0), "% CI [", round(x$estimator$lower, 3), ", ",
             round(x$estimator$upper, 3), "]\n"))
  cat(paste0("SESOI: [", round(x$test$SESOI_lower, 3), ", ", round(x$test$SESOI_upper, 3), "]"))
  cat("\n\n")

  met <- data.frame(
    Test = x$test$test,
    prob = x$results$p_value)

  print(met, row.names = FALSE)

  cat("\n")
  cat(paste0("Final inference: ", x$results$inference))
}


# =======================================================
#' S3 method for plotting \code{\link{bootstrap_MBI}} results
#' @param x Object of class \code{bmbstats_MBI}
#' @param ... Extra arguments. Use \code{plot_control} for plotting options, particularly
#'     \code{effect_colors} parameter
#' @export
#' @examples
#' mean_MBI <- bootstrap_MBI(
#'   describe_data(rnorm(10, 100, 20)),
#'   estimator = "mean",
#'   SESOI_lower = 95,
#'   SESOI_upper = 105
#' )
#' mean_MBI
#' plot(mean_MBI, control = plot_control(points_jitter = FALSE))
plot.bmbstats_MBI <- function(x, ...) {
  plot_MBI_null_distribution(x, ...)
}


# ----------------------------------------------------------------------------
plot_MBI_null_distribution <- function(x, control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  estimator <- NULL
  ..x.. <- NULL
  xmin <- NULL
  xmax <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  # Extract data from bootstrap_MBI_out
  estimator_name <- x$estimator$name
  estimator_value <- x$estimator$value
  estimator_lower <- x$estimator$lower
  estimator_upper <- x$estimator$upper
  confidence <- x$estimator$confidence
  p_value <- x$result$p_value

  plot_df <- data.frame(
    estimator = x$distribution$estimator_distribution$estimator
  )

  SESOI_lower <- x$test$SESOI_lower
  SESOI_upper <- x$test$SESOI_upper
  final_inference <- x$result$inference

  estimator_plot_df <- data.frame(
    estimator = estimator_value,
    xmax = estimator_upper,
    xmin = estimator_lower,
    y = 1
  )

  gg <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(y = 1, x = estimator, group = 1)
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggridges::geom_density_ridges_gradient(
      ggplot2::aes(
        fill = get_magnitude(
          ..x..,
          SESOI_lower,
          SESOI_upper
        )),
      jittered_points = control$points_jitter,
      quantile_lines = control$cloud_quantile_lines,
      scale = control$cloud_scale,
      color = control$cloud_color,
      vline_size = control$cloud_quantile_lines_size,
      vline_color = control$cloud_quantile_lines_color,
      point_shape = control$points_shape,
      point_size = control$points_size,
      point_alpha = control$points_alpha,
      position = ggridges::position_raincloud(
        adjust_vlines = control$cloud_quantile_lines_adjust,
        ygap = control$points_gap,
        height = control$points_jitter_width
      )
    ) +
    ggplot2::annotate(
      "rect",
      xmin = SESOI_lower,
      xmax = SESOI_upper,
      ymin = -Inf,
      ymax = Inf,
      alpha = control$SESOI_alpha,
      fill = control$SESOI_color
    ) +
    ggstance::geom_pointrangeh(
      data = estimator_plot_df,
      ggplot2::aes(xmin = xmin, xmax = xmax),
      #position = ggplot2::position_nudge(y = control$summary_bar_nudge),
      size = control$summary_bar_size,
      color = control$summary_bar_color
    ) +
    ggplot2::ylab(NULL) +
    # ggplot2::xlab(NULL) +
    ggplot2::scale_discrete_manual(
      aesthetics = c("fill"),
      values = control$effect_colors,
      drop = FALSE,
      limits = levels(get_magnitude(0, 0, 0))
    ) +
    ggplot2::theme(
      legend.position = control$legend_position,
       legend.title = ggplot2::element_blank()) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::xlab(estimator_name) +
    ggplot2::ggtitle(
      paste(
        estimator_name, " = ",
        round(estimator_value, 2),
        "\n",
        round(confidence * 100, 0),
        "% CI [",
        round(estimator_lower, 2),
        ", ",
        round(estimator_upper, 2),
        "]",
        sep = ""
      ),
      paste(
        "Inference: ",
        final_inference,
        " [",
        round(p_value[1] * 100, 0),
        "/",
        round(p_value[2] * 100, 0),
        "/",
        round(p_value[3] * 100, 0),
        "]",
        sep = ""
      )
    )

  return(gg)
  }
