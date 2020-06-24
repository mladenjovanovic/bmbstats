#' Spaghetti plot
#'
#' @inheritParams plot_raincloud
#' @inheritParams basic_arguments
#' @param id Character string. Name of  the column in \code{data}
#' @param observations Character string. Name of the column in \code{data}
#' @param observations_label Character string. Label to be used for x-axis. Default is \code{observations}
#' @return \code{\link[ggplot2]{ggplot}} object
#' @export
#' @examples
#' test_data <- expand.grid(
#'   id = 1:10,
#'   obs = 1:10
#' )
#'
#' test_data$val <- with(
#'   test_data,
#'   rnorm(nrow(test_data), obs, id)
#' )
#'
#' plot_spaghetti(
#'   test_data,
#'   id = "id",
#'   observations = "obs",
#'   value = "val",
#'   SESOI_lower = -1,
#'   SESOI_upper = 1,
#'   control = plot_control(
#'     points_shape = 21,
#'     points_fill = "white",
#'     points_alpha = 1,
#'     points_size = 0.75,
#'     line_size = 0.75,
#'     legend_position = "right"
#'   )
#' )
plot_spaghetti <- function(data,
                           id,
                           observations,
                           observations_label = observations,
                           value,
                           value_label = value,
                           SESOI_lower = 0,
                           SESOI_upper = 0,
                           control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  x <- NULL
  y <- NULL
  xend <- NULL
  yend <- NULL
  magnitude <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  # Prepare data for plotting
  plot_data <- data.frame(
    id = data[[id]],
    x = data[[observations]],
    y = data[[value]]
  )

  # Check if x is factor
  x_factor <- is.factor(plot_data$x)

  # Sort
  plot_data <- plot_data[with(
    plot_data,
    order(id, x)
  ), ]

  # Split the data
  plot_data.id <- split(plot_data, plot_data$id)

  # Create segments
  plot_data.seg <- lapply(
    plot_data.id,
    function(id) {
      segments_x <- stats::embed(as.vector(id$x), 2)
      segments_y <- stats::embed(as.vector(id$y), 2)

      data.frame(
        id = id$id[[1]],
        yend = segments_y[, 1],
        y = segments_y[, 2],
        xend = segments_x[, 1],
        x = segments_x[, 2]
      )
    }
  )
  # Merge into single DF
  plot_data.seg <- do.call(rbind, plot_data.seg)

  # Convert back to factor if needed
  if (x_factor) {
    plot_data.seg$xend <- factor(plot_data.seg$xend, levels = levels(plot_data$x))
    plot_data.seg$x <- factor(plot_data.seg$x, levels = levels(plot_data$x))
  }

  # Create magnitude
  plot_data.seg$change <- with(
    plot_data.seg,
    yend - y
  )

  # Get the effect magnitudes
  plot_data.seg$magnitude <- get_magnitude(
    plot_data.seg$change,
    SESOI_lower,
    SESOI_upper
  )

  gg <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = x, y = y)
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::geom_segment(
      data = plot_data.seg,
      ggplot2::aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend,
        color = magnitude
      ),
      alpha = control$line_alpha,
      size = control$line_size
    ) +
    ggplot2::scale_discrete_manual(
      aesthetics = c("color"),
      values = control$effect_colors,
      drop = FALSE,
      limits = levels(plot_data.seg$magnitude)
    ) +
    # Add points
    ggplot2::geom_point(
      alpha = control$points_alpha,
      shape = control$points_shape,
      size = control$points_size,
      color = control$points_color,
      fill = control$points_fill
    ) +
    ggplot2::theme(
      legend.position = control$legend_position,
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::ylab(value_label) +
    ggplot2::xlab(observations_label)

  if (x_factor) {
    gg <- gg + ggplot2::scale_x_discrete(drop = FALSE)
  }

  return(gg)
}
