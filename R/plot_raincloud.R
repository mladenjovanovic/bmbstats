# =============================================================================
#' Raincloud plot
#'
#' @param data Data frame
#' @param value Character string. Name of  the column in \code{data}
#' @param value_label Character string. Label to be used. Default is \code{value}
#' @param groups Character string. Name of  the column in \code{data}. Default is \code{NULL}
#' @param control Plotting control object returned from \code{\link{plot_control}}
#'
#' @return \code{\link[ggplot2]{ggplot2}} object
#' @seealso \code{\link{plot_raincloud_SESOI}}
#' @export
#'
#' @examples
#' plot_raincloud(data.frame(data = rnorm(100)), value = "data")
#' plot_raincloud(
#'   iris,
#'   value = "Sepal.Length",
#'   groups = "Species",
#'   control = plot_control(points_size = 1)
#' )
plot_raincloud <- function(data,
                           value,
                           value_label = value,
                           groups = NULL,
                           control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  ..ndensity.. <- NULL
  x <- NULL
  y <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  # Prepare data for plotting
  plot_data <- data.frame(
    y = data[[value]]
  )

  # Check if x is missing
  missing_x <- FALSE

  if (is.null(groups)) {
    plot_data$x <- ""
    missing_x <- TRUE
  } else {
    plot_data$x <- data[[groups]]
  }

  # Raincloud
  gg_raincloud <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      y = x,
      x = y,
      fill = x,
      point_color = x
    )
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggridges::geom_density_ridges(
      ggplot2::aes(
        height = ..ndensity..
      ),
      jittered_points = control$points_jitter,
      quantile_lines = control$cloud_quantile_lines,
      scale = control$cloud_scale,
      alpha = control$cloud_alpha,
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
    ggstance::stat_summaryh(
      fun.data = mean_sd_h,
      geom = "pointrangeh",
      position = ggplot2::position_nudge(y = control$summary_bar_nudge),
      size = control$summary_bar_size,
      color = control$summary_bar_color
    ) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(value_label) +
    ggplot2::scale_discrete_manual(
      aesthetics = c("point_color", "fill"),
      values = control$group_colors
    ) +
    ggplot2::theme(
      legend.position = control$legend_position,
      legend.title = ggplot2::element_blank()
    )


  # Flip the chart
  if (control$coord_flip) {
    gg_raincloud <- gg_raincloud + ggplot2::coord_flip()

    if (missing_x) {
      gg_raincloud <- gg_raincloud +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.line.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )
    }
  } else {
    if (missing_x) {
      gg_raincloud <- gg_raincloud +
        ggplot2::theme(
          axis.title.y = ggplot2::element_blank(),
          axis.line.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    }
  }

  gg_raincloud
}

# =============================================================================
#' Raincloud plot with smallest effect size of interest
#' @inheritParams plot_raincloud
#' @inheritParams basic_arguments
#' @export
#' @seealso \code{\link{plot_raincloud}}
#' @return \code{\link[ggplot2]{ggplot2}} object
#' @examples
#' plot_raincloud_SESOI(
#'   iris,
#'   value = "Sepal.Length",
#'   groups = "Species",
#'   SESOI_lower = 5,
#'   SESOI_upper = 7
#' )
plot_raincloud_SESOI <- function(data,
                                 value,
                                 value_label = value,
                                 groups = NULL,
                                 SESOI_lower = 0,
                                 SESOI_upper = 0,
                                 control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  ..ndensity.. <- NULL
  ..x.. <- NULL
  x <- NULL
  y <- NULL
  SESOI_Effects <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  # Prepare data for plotting
  plot_data <- data.frame(
    y = data[[value]]
  )

  # Check if x is missing
  missing_x <- FALSE

  if (is.null(groups)) {
    plot_data$x <- ""
    missing_x <- TRUE
  } else {
    plot_data$x <- data[[groups]]
  }

  # Get the effect magnitudes
  plot_data$SESOI_Effects <- get_magnitude(
    plot_data$y,
    SESOI_lower,
    SESOI_upper
  )

  # Raincloud
  gg_raincloud <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      y = x,
      x = y,
      group = x,
      point_color = SESOI_Effects
    )
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggridges::geom_density_ridges_gradient(
      ggplot2::aes(
        height = ..ndensity..,
        fill = get_magnitude(
          ..x..,
          SESOI_lower,
          SESOI_upper
        )
      ),
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
      fill = control$effect_colors[2]
    ) +
    ggstance::stat_summaryh(
      fun.data = mean_sd_h,
      geom = "pointrangeh",
      position = ggplot2::position_nudge(y = control$summary_bar_nudge),
      size = control$summary_bar_size,
      color = control$summary_bar_color
    ) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(value_label) +
    ggplot2::scale_discrete_manual(
      aesthetics = c("point_color", "fill"),
      values = control$effect_colors,
      drop = FALSE,
      limits = levels(plot_data$SESOI_Effects)
    ) +
    ggplot2::theme(
      legend.position = control$legend_position,
      legend.title = ggplot2::element_blank()
    )

  # Flip the chart
  if (control$coord_flip) {
    gg_raincloud <- gg_raincloud + ggplot2::coord_flip()

    if (missing_x) {
      gg_raincloud <- gg_raincloud +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.line.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )
    }
  } else {
    if (missing_x) {
      gg_raincloud <- gg_raincloud +
        ggplot2::theme(
          axis.title.y = ggplot2::element_blank(),
          axis.line.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    }
  }

  gg_raincloud
}
