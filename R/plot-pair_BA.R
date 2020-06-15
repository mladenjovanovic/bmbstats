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
#' plot_pair_BA(practical,
#'   criterion,
#'   SESOI_lower = -10,
#'   SESOI_upper = 10,
#'   predictor_label = "Practical",
#'   outcome_label = "Criterion"
#' )
plot_pair_BA <- function(predictor,
                         outcome,
                         SESOI_lower = 0,
                         SESOI_upper = 0,
                         confidence = 0.95,
                         predictor_label = "Predictor",
                         outcome_label = "Outcome",
                         control = plot_control(),
                         na.rm = FALSE) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  midway <- NULL
  difference <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  if (length(predictor) != length(outcome)) {
    stop("Predictor and Outcome variables differ in size. Unable to proceed", call. = FALSE)
  }

  # Prepare DF for plotting
  plot_data <- data.frame(
    predictor = predictor,
    outcome = outcome
  )

  if (na.rm) plot_data <- stats::na.omit(plot_data)

  n_observations <- nrow(plot_data)

  plot_data$difference <- plot_data$outcome - plot_data$predictor
  plot_data$midway <- (plot_data$predictor + plot_data$outcome) / 2

  plot_data$SESOI_lower <- plot_data$predictor + SESOI_lower
  plot_data$SESOI_upper <- plot_data$predictor + SESOI_upper

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
      x = predictor,
      y = outcome
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
    ggplot2::xlab(predictor_label) +
    ggplot2::ylab(outcome_label)


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
        predictor_label,
        " + ",
        outcome_label,
        ") / 2",
        sep = ""
      )
    ) +
    ggplot2::ylab(
      paste(
        outcome_label,
        " - ",
        predictor_label,
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
