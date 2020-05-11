#' Bland-Altman plot
#'
#' @inheritParams basic_arguments
#' @inheritParams plot_raincloud
#' @return \code{\link[ggplot2]{ggplot}} object
#' @export
#' @seealso \code{\link{plot_pair_lm}} for lm residuals plot and
#'    \code{\link{plot_pair_OLP}} for ordinary-least-squares residuals
#'     plot
#' @examples
#' criterion <- rnorm(
#'   n = 100,
#'   mean = 100,
#'   sd = 10
#' )
#'
#' practical <- criterion * 1.2 + rnorm(n = 100, mean = -12, sd = 5)
#'
#' comment(practical) <- "practical"
#' comment(criterion) <- "criterion"
#'
#' plot_pair_BA(practical,
#'   criterion,
#'   SESOI_lower = -10,
#'   SESOI_upper = 10
#' )
plot_pair_BA <- function(group_a,
                         group_b,
                         SESOI_lower = 0,
                         SESOI_upper = 0,
                         confidence = 0.95,
                         control = plot_control(),
                         na.rm = FALSE) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  midway <- NULL
  difference <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  if (length(group_a) != length(group_b)) {
    stop("Group A and Group B differ in size. Unable to proceed")
  }

  group_a_label <- comment(group_a)
  group_b_label <- comment(group_b)

  if (is.null(group_a_label)) group_a_label <- "Group A"
  if (is.null(group_b_label)) group_b_label <- "Group B"

  # Prepare DF for plotting
  plot_data <- data.frame(
    group_a = group_a,
    group_b = group_b
  )

  if (na.rm) plot_data <- stats::na.omit(plot_data)

  n_observations <- nrow(plot_data)

  plot_data$difference <- plot_data$group_b - plot_data$group_a
  plot_data$midway <- (plot_data$group_a + plot_data$group_b) / 2

  plot_data$SESOI_lower <- plot_data$group_a + SESOI_lower
  plot_data$SESOI_upper <- plot_data$group_a + SESOI_upper

  # Level of agreement
  mean_difference <- mean(plot_data$difference)
  sd_difference <- stats::sd(plot_data$difference)
  LOA <- sd_difference * stats::qt(
    1 - ((1 - confidence) / 2),
    df = n_observations - 1
  )
  LOA_lower <- mean_difference - LOA
  LOA_upper <- mean_difference + LOA

  # Scatter plot
  bland_altman_A <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = group_a,
      y = group_b
    )
  ) +
    cowplot::theme_cowplot(control$font_size)
  # Identity line
  if (control$identity_line) {
    if (control$SESOI_band) {
      bland_altman_A <- bland_altman_A +
        # SESOI band around identity line
        ggplot2::geom_ribbon(
          ggplot2::aes(
            ymin = SESOI_lower,
            ymax = SESOI_upper
          ),
          fill = control$SESOI_color,
          alpha = control$SESOI_alpha
        )
    }
    bland_altman_A <- bland_altman_A +
      ggplot2::geom_abline(
        slope = 1,
        intercept = 0,
        colour = "black",
        linetype = "dashed",
        alpha = 0.4
      )
  }
  bland_altman_A <- bland_altman_A +
    ggplot2::geom_point(
      alpha = control$points_alpha,
      size = control$points_size,
      shape = control$points_shape,
      color = control$points_color,
      fill = control$points_fill
    ) +
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
    ggplot2::xlab(group_a_label) +
    ggplot2::ylab(group_b_label)


  bland_altman_B <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = midway,
      y = difference
    )
  ) +
    cowplot::theme_cowplot(control$font_size)

  # SESOI band
  if (control$SESOI_band) {
    bland_altman_B <- bland_altman_B +
      ggplot2::annotate(
        "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = SESOI_lower,
        ymax = SESOI_upper,
        fill = control$SESOI_color,
        alpha = control$SESOI_alpha
      )
  }

  bland_altman_B <- bland_altman_B +
    ggplot2::geom_abline(
      slope = 0,
      intercept = 0,
      color = "white",
      size = 0.75
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

    # Regression line
    ggplot2::geom_smooth(
      formula = y ~ x,
      method = control$smooth_method,
      level = confidence,
      se = control$smooth_se,
      size = control$smooth_size,
      alpha = control$smooth_alpha,
      color = control$smooth_color,
      fill = control$smooth_fill
    ) +
    ggplot2::xlab(
      paste(
        "(",
        group_a_label,
        " + ",
        group_b_label,
        ") / 2",
        sep = ""
      )
    ) +
    ggplot2::ylab(
      paste(
        group_b_label,
        " - ",
        group_a_label,
        sep = ""
      )
    )

  bland_altman <- cowplot::plot_grid(
    bland_altman_A,
    bland_altman_B,
    labels = control$panel_labels,
    label_size = control$panel_labels_size,
    hjust = c(0, 0, 0),
    align = "hv",
    axis = "l",
    ncol = 2,
    nrow = 1
  )

  return(bland_altman)
}
