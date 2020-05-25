# =======================================================
#' S3 method for printing \code{\link{RCT_analysis}} results
#' @param x Object of class \code{bmbstats_RCT_analysis}
#' @param ... Extra arguments. Not used
#' @export
#' @examples
#' set.seed(1666)
#'
#' data("vertical_jump_data")
#'
#' rct_model <- RCT_analysis(
#'   vertical_jump_data,
#'   group = "Group",
#'   treatment_label = "Treatment",
#'   control_label = "Control",
#'   pre_test = "Pre-test",
#'   post_test = "Post-test",
#'   control = model_control(boot_type = "perc", boot_samples = 1000)
#' )
#'
#' rct_model
#'
#' plot(rct_model)
print.bmbstats_RCT_analysis <- function(x, ...) {
  cat(
    paste0("Bootstrap with ", x$control$boot_samples, " resamples and ", x$control$confidence * 100, "% ",
           x$control$boot_type, " confidence intervals.\n\n")
  )

  print(x$estimators)
}


#' S3 method for plotting \code{\link{RCT_analysis}} results
#'
#'
#' @param x Object of class \code{bmbstats_RCT_analysis}
#' @param type Type of plot. Options are "boot",.  Default is "boot"
#' @param ... Extra arguments. Use \code{\link{plot_control}} to control plotting style
#' @export
#' @examples
#' set.seed(1666)
#'
#' data("vertical_jump_data")
#'
#' rct_model <- RCT_analysis(
#'   vertical_jump_data,
#'   group = "Group",
#'   treatment_label = "Treatment",
#'   control_label = "Control",
#'   pre_test = "Pre-test",
#'   post_test = "Post-test",
#'   control = model_control(boot_type = "perc", boot_samples = 1000)
#' )
#'
#' rct_model
#'
#' plot(rct_model)
plot.bmbstats_RCT_analysis <- function(x, type = "boot", ...) {

  rlang::arg_match(type, c(
    "boot",
    "control-pre-post",
    "treatment-pre-post",
    "control-change",
    "treatment-change",
    "change",
    "control-paired-change",
    "treatment-paired-change",
    "change-distribution",
    "effect-distribution"
  ))

  gg <- list(NULL)

  # Bootstrap distribution
  if (type == "boot") {
     class(x) <- "bmbstats"
     gg <- plot(x, ...)
  }

  # Pre-Post graph
  if (type == "control-pre-post") {
     gg <- RCT_plot_control_pre_post(x, ...)
  }

  # Pre-Post graph
  if (type == "treatment-pre-post") {
    gg <- RCT_plot_treatment_pre_post(x, ...)
  }

  # Change graph
  if (type == "control-change") {
    gg <- RCT_plot_control_change(x, ...)
  }

  # Change graph
  if (type == "treatment-change") {
    gg <- RCT_plot_treatment_change(x, ...)
  }

  # Change graph
  if (type == "change") {
    gg <- RCT_plot_change(x, ...)
  }

  # Paired graph
  if (type == "control-paired-change") {
    gg <- RCT_plot_control_paired_change(x, ...)
  }

  # Paired graph
  if (type == "treatment-paired-change") {
    gg <- RCT_plot_treatment_paired_change(x, ...)
  }

  # Change distribution
  if (type == "change-distribution") {
    gg <- RCT_plot_change_distribution(x, ...)
  }

  # Effect distribution
  if (type == "effect-distribution") {
    gg <- RCT_plot_effect_distribution(x, ...)
  }

  return(gg)

}

# ---------------------------------------------------------
RCT_plot_control_pre_post <- function(x, control = plot_control()) {
  plot_data <- dplyr::tibble(
    `Pre-test` = x$extra$control_pre_test,
    `Post-test` = x$extra$control_post_test)

  plot_data <- tidyr::gather(plot_data)

  plot_data$key <- factor(
    plot_data$key,
    levels = c("Pre-test", "Post-test"),
    labels = c(x$extra$pre_test_label, x$extra$post_test_label)
  )

  plot_raincloud(
    data = plot_data,
    value = "value",
    value_label = NULL,
    groups = "key",
    control = control)
}

# ---------------------------------------------------------
RCT_plot_treatment_pre_post <- function(x, control = plot_control()) {
  plot_data <- dplyr::tibble(
    `Pre-test` = x$extra$treatment_pre_test,
    `Post-test` = x$extra$treatment_post_test)

  plot_data <- tidyr::gather(plot_data)

  plot_data$key <- factor(
    plot_data$key,
    levels = c("Pre-test", "Post-test"),
    labels = c(x$extra$pre_test_label, x$extra$post_test_label)
  )

  plot_raincloud(
    data = plot_data,
    value = "value",
    value_label = NULL,
    groups = "key",
    control = control)
}

# ---------------------------------------------------------
RCT_plot_control_change <- function(x, control = plot_control()) {

  plot_data <- data.frame(
    change = x$extra$control_change
  )

  plot_raincloud_SESOI(
    data = plot_data,
    value = "change",
    value_label = NULL,
    SESOI_lower = x$extra$SESOI_lower,
    SESOI_upper = x$extra$SESOI_upper,
    control = control)
}

# ---------------------------------------------------------
RCT_plot_treatment_change <- function(x, control = plot_control()) {

  plot_data <- data.frame(
    change = x$extra$treatment_change
  )

  plot_raincloud_SESOI(
    data = plot_data,
    value = "change",
    value_label = NULL,
    SESOI_lower = x$extra$SESOI_lower,
    SESOI_upper = x$extra$SESOI_upper,
    control = control)
}


# ---------------------------------------------------------
RCT_plot_change <- function(x, control = plot_control()) {

  plot_data <- data.frame(
    control = x$extra$control_change,
    treatment = x$extra$treatment_change
  )

  plot_data <- tidyr::gather(plot_data)

  plot_data$key <- factor(
    plot_data$key,
    levels = c("control", "treatment"),
    labels = c(x$extra$control_label, x$extra$treatment_label)
  )

  plot_raincloud_SESOI(
    data = plot_data,
    value = "value",
    value_label = NULL,
    groups = "key",
    SESOI_lower = x$extra$SESOI_lower,
    SESOI_upper = x$extra$SESOI_upper,
    control = control)
}


# ---------------------------------------------------------
RCT_plot_control_paired_change <- function(x, control = plot_control()) {

  plot_data <- dplyr::tibble(
    `Pre-test` = x$extra$control_pre_test,
    `Post-test` = x$extra$control_post_test)

  plot_pair_changes(
    group_a = plot_data$`Pre-test`,
    group_a_label = x$extra$pre_test_label,
    group_b = plot_data$`Post-test`,
    group_b_label = x$extra$post_test_label,
    SESOI_lower = x$extra$SESOI_lower,
    SESOI_upper = x$extra$SESOI_upper,
    control = control
  )

}

# ---------------------------------------------------------
RCT_plot_treatment_paired_change <- function(x, control = plot_control()) {

  plot_data <- dplyr::tibble(
    `Pre-test` = x$extra$treatment_pre_test,
    `Post-test` = x$extra$treatment_post_test)

  plot_pair_changes(
    group_a = plot_data$`Pre-test`,
    group_a_label = x$extra$pre_test_label,
    group_b = plot_data$`Post-test`,
    group_b_label = x$extra$post_test_label,
    SESOI_lower = x$extra$SESOI_lower,
    SESOI_upper = x$extra$SESOI_upper,
    control = control
  )

}

# ---------------------------------------------------------
RCT_plot_change_distribution <- function(x, control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  value <- NULL
  key <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  plot_data <- data.frame(
    control = x$extra$control_change,
    treatment = x$extra$treatment_change
  )


  min_change <- min(plot_data$control, plot_data$treatment)
  max_change <- max(plot_data$control, plot_data$treatment)
  range_change <- max_change - min_change

  plot_data <- tidyr::gather(plot_data)

  plot_data$key <- factor(
    plot_data$key,
    levels = c("control", "treatment"),
    labels = c(x$extra$control_label, x$extra$treatment_label)
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = value, fill = key)) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::geom_density(
      color = control$cloud_color,
      alpha = control$cloud_alpha, trim = FALSE) +
    ggplot2::annotate(
      "rect",
      xmin = x$extra$SESOI_lower,
      xmax = x$extra$SESOI_upper,
      ymin = -Inf,
      ymax = Inf,
      alpha = control$SESOI_alpha,
      fill = control$SESOI_color) +
    ggplot2::geom_vline(xintercept = 0, color = control$SESOI_color) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL) +
    ggplot2::scale_fill_manual(values = control$group_colors) +
    ggplot2::theme(
      legend.position = "none",
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::xlim(c(
      min_change - 0.25 * range_change,
      max_change + 0.25 * range_change))
}

# ---------------------------------------------------------
RCT_plot_effect_distribution <- function(x, control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  ..ndensity.. <- NULL
  ..x.. <- NULL
  x <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  # Treatment effects
  systematic_effect <- (mean(x$extra$treatment_change) - mean(x$extra$control_change))
  random_effect <- sqrt(stats::var(x$extra$treatment_change) - stats::var(x$extra$control_change))

  plot_data <- data.frame(
    x = perfect_rnorm(
    n = 1000,
    mean = systematic_effect,
    sd = random_effect
  ))

  SESOI_lower <- x$extra$SESOI_lower
  SESOI_upper <- x$extra$SESOI_upper

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      y = 1,
      x = x
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
      jittered_points = FALSE,
      quantile_lines = control$cloud_quantile_lines,
      scale = control$cloud_scale,
      color = control$cloud_color,
      vline_size = control$cloud_quantile_lines_size,
      vline_color = control$cloud_quantile_lines_color,
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
    ggplot2::geom_vline(xintercept = 0, color = control$SESOI_color) +
    ggstance::stat_summaryh(
      fun.data = mean_sd_h,
      geom = "pointrangeh",
      position = ggplot2::position_nudge(y = control$summary_bar_nudge),
      size = control$summary_bar_size,
      color = control$summary_bar_color
    ) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL) +
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

}
