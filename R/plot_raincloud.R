#' Plot control constructor
#'
#' Returns default plotting parameters for \code{\link{plot_raincloud}},
#'    \code{\link{plot_raincloud_SESOI}}, and \code{\link{plot_pair_changes}}
#'
#' @param group_colors Default groups plotting colors
#' @param effect_colors Default effects plotting colors (i.e., Lower, Equivalent, Higher)
#' @param cloud_scale Scale of the cloud
#' @param cloud_alpha Cloud alpha/transparency
#' @param cloud_color Border color of the cloud
#' @param cloud_quantile_lines Should quantile lines be plotted? Default is TRUE
#' @param cloud_quantile_lines_size Size of the quantile lines
#' @param cloud_quantile_lines_color Color of the quantile lines
#' @param cloud_quantile_lines_adjust Should quantile lines be extended?
#' @param points_shape Point shape. Default is 21
#' @param points_color Point color. Default is "black"
#' @param points_fill Point fill. Default is "transparent"
#' @param points_size Point size
#' @param points_alpha Transparency of the points
#' @param points_jitter Should jitter be used?
#' @param points_jitter_width Width of the jitter
#' @param points_gap Gap between points and cloud
#' @param summary_bar_nudge Gap between summary bar and cloud
#' @param summary_bar_size Size of the summary bar
#' @param summary_bar_color Color of the summary bar
#' @param line_size Line size
#' @param line_color Line color. Default is "black"
#' @param line_alpha Line transparency. Default is 0.5
#' @param SESOI_alpha Transparency for the SESOI band. Default is 0.2
#' @param font_size Overall size of fonts
#' @param legend_position Position of the legend. Default is "none". See \code{\link[ggplot2]{theme}}
#'     for more info
#' @param coord_flip Should figure be flipped?
#'
#' @return List with plot control elements
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
plot_control <- function(group_colors = c(
                           "#5DA5DA",
                           "#F15854",
                           "#4D4D4D",
                           "#60BD68",
                           "#FAA43A",
                           "#F17CB0",
                           "#B276B2",
                           "#DECF3F"
                         ),
                         effect_colors = c(
                           Lower = "#F158546A",
                           Equivalent = "#4D4D4D6A",
                           Higher = "#60BD686A"
                         ),
                         cloud_scale = 0.5,
                         cloud_alpha = 0.6,
                         cloud_color = NA,
                         cloud_quantile_lines = TRUE,
                         cloud_quantile_lines_size = 0.4,
                         cloud_quantile_lines_color = "white",
                         cloud_quantile_lines_adjust = FALSE,
                         points_shape = 21,
                         points_color = "black",
                         points_fill = "transparent",
                         points_size = 2,
                         points_alpha = 0.5,
                         points_jitter = TRUE,
                         points_jitter_width = 0.1,
                         points_gap = 0.1,
                         summary_bar_nudge = 0.075,
                         summary_bar_size = 1,
                         summary_bar_color = "black",
                         line_size = 1,
                         line_color = "black",
                         line_alpha = 0.5,
                         SESOI_alpha = 0.2,
                         font_size = 8,
                         legend_position = "none",
                         coord_flip = TRUE) {
  list(
    group_colors = group_colors,
    effect_colors = effect_colors,
    cloud_scale = cloud_scale,
    cloud_alpha = cloud_alpha,
    cloud_color = cloud_color,
    cloud_quantile_lines = cloud_quantile_lines,
    cloud_quantile_lines_size = cloud_quantile_lines_size,
    cloud_quantile_lines_color = cloud_quantile_lines_color,
    cloud_quantile_lines_adjust = cloud_quantile_lines_adjust,
    points_shape = points_shape,
    points_color = points_color,
    points_fill = points_fill,
    points_size = points_size,
    points_alpha = points_alpha,
    points_jitter = points_jitter,
    points_jitter_width = points_jitter_width,
    points_gap = points_gap,
    summary_bar_nudge = summary_bar_nudge,
    summary_bar_size = summary_bar_size,
    summary_bar_color = summary_bar_color,
    line_size = line_size,
    line_color = line_color,
    line_alpha = line_alpha,
    SESOI_alpha = SESOI_alpha,
    font_size = font_size,
    legend_position = legend_position,
    coord_flip = coord_flip
  )
}

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
