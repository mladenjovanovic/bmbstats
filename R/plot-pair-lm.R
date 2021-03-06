#' Linear model plot
#'
#' This function is used in reliability and validity visualization
#'
#' @inheritParams basic_arguments
#' @inheritParams plot_raincloud
#' @return \code{\link[ggplot2]{ggplot}} object
#' @export
#' @seealso \code{\link{plot_pair_BA}} for Bland-Altman plot and
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
#' plot_pair_lm(practical,
#'   criterion,
#'   SESOI_lower = -10,
#'   SESOI_upper = 10,
#'   predictor_label = "Practical",
#'   outcome_label = "Criterion"
#' )
plot_pair_lm <- function(predictor,
                         outcome,
                         SESOI_lower = 0,
                         SESOI_upper = 0,
                         confidence = 0.95,
                         predictor_label = "Predictor",
                         outcome_label = "Outcome",
                         fitted_label = "Fitted",
                         residuals_label = "Residuals",
                         control = plot_control(),
                         na.rm = FALSE) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  x <- NULL
  y <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  if (length(predictor) != length(outcome)) {
    stop("Predictor and Outcome variables differ in size. Unable to proceed", call. = FALSE)
  }

  # Prepare DF
  plot_data <- data.frame(
    x = predictor,
    y = outcome
  )

  if (na.rm) plot_data <- stats::na.omit(plot_data)
  n_observations <- nrow(plot_data)

  lm_model <- stats::lm(
    y ~ x,
    plot_data
  )

  lm_model_rse <- summary(lm_model)$sigma
  y_fitted <- stats::fitted(lm_model)
  y_resid <- stats::resid(lm_model)

  plot_data <- data.frame(
    x = plot_data$x,
    y = plot_data$y,
    y_fitted = y_fitted,
    y_resid = y_resid
  )

  # Level of agreement
  mean_difference <- mean(plot_data$y_resid)
  LOA <- lm_model_rse * stats::qt(
    1 - ((1 - confidence) / 2),
    df = n_observations - 1
  )
  LOA_lower <- mean_difference - LOA
  LOA_upper <- mean_difference + LOA

  # Scatterplot
  gg_scatter <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(y = y, x = x)
  ) +
    cowplot::theme_cowplot(control$font_size)

  # Identity line
  if (control$identity_line) {
    gg_scatter <- gg_scatter +
      ggplot2::geom_abline(
        slope = 1,
        intercept = 0,
        colour = "black",
        linetype = "dashed",
        alpha = 0.4
      )
  }

  gg_scatter <- gg_scatter + ggplot2::geom_point(
    alpha = control$points_alpha,
    size = control$points_size,
    shape = control$points_shape,
    color = control$points_color,
    fill = control$points_fill
  ) +

    ggplot2::geom_abline(
      intercept = stats::coef(lm_model)[[1]],
      slope = stats::coef(lm_model)[[2]]
    ) +
    ggplot2::labs(x = predictor_label, y = outcome_label)

  # Residuals plot
  gg_resid <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(y = y_resid, x = y_fitted)
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
    ggplot2::labs(x = fitted_label, y = residuals_label)

  cowplot::plot_grid(
    gg_scatter,
    gg_resid,
    labels = control$panel_labels,
    label_size = control$panel_labels_size,
    hjust = c(0, 0, 0),
    align = "hv",
    axis = "l",
    ncol = 2,
    nrow = 1
  )
}
