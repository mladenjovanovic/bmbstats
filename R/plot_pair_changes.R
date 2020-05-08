#' Plot pair changes
#' @details To provide labels from groups, use \code{comment} attribute
#' @inheritParams basic_arguments
#' @inheritParams plot_raincloud
#' @param value_label Character string. Label to be used for y-axis. Default is \code{NULL}
#' @param group_label Character string. Label to be used for x-axis. Default is \code{NULL}
#' @return Returns \code{link[ggplot2]{ggplot}} object
#' @export
#' @examples
#' plot_pair_changes(rnorm(100, 100, 20),
#'   rnorm(100, 110, 40),
#'   SESOI_lower = -10,
#'   SESOI_upper = 10
#' )
plot_pair_changes <- function(group_a,
                              group_b,
                              value_label = NULL,
                              group_label = NULL,
                              SESOI_lower = 0,
                              SESOI_upper = 0,
                              control = plot_control()) {
  
  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  group <- NULL
  id <- NULL
  magnitude <- NULL
  value <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++
  
  if (length(group_a) != length(group_b)) {
    stop("Group A and Group B differ in size. Unable to proceed")
  }

  group_a_label <- comment(group_a)
  group_b_label <- comment(group_b)


  if (is.null(group_a_label)) group_a_label <- "Group A"
  if (is.null(group_b_label)) group_b_label <- "Group B"

  n_obs <- length(group_a)
  change <- group_b - group_a
  
  change_magnitude <- get_magnitude(change, SESOI_lower, SESOI_upper)

  data <- rbind(
    data.frame(
      id = seq(1, n_obs),
      group = group_a_label,
      value = group_a,
      magnitude = change_magnitude
    ),
    data.frame(
      id = seq(1, n_obs),
      group = group_b_label,
      value = group_b,
      magnitude = change_magnitude
    )
  )

  data$group <- factor(
    data$group,
    levels = c(group_a_label, group_b_label),
    ordered = TRUE
  )

  gg <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = group,
      y = value,
      group = id,
      color = magnitude
    )
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::geom_line(alpha = control$line_alpha) +
    ggplot2::geom_point(alpha = control$points_alpha) +
    ggplot2::scale_color_manual(
      values = control$effect_colors,
      drop = FALSE,
      limits = levels(change_magnitude)
    ) +
    ggplot2::ylab(value_label) +
    ggplot2::theme(
      legend.position = control$legend_position,
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::xlab(group_label)

  return(gg)
}
