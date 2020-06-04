# =======================================================
#' S3 method for printing \code{\link{bootstrap_MET}} results
#' @param x Object of class \code{bmbstats_MET}
#' @param ... Extra arguments. Not used
#' @export
#' @examples
#' mean_MET <- bootstrap_MET(
#'   describe_data(rnorm(10, 100, 20)),
#'   estimator = "mean",
#'   SESOI_lower = 95,
#'   SESOI_upper = 105,
#'   alpha = 0.05
#' )
#' mean_MET
#' plot(mean_MET)
print.bmbstats_MET <- function(x, ...) {

  cat(paste0("Minimum effect tests for the `", x$estimator$name,"` estimator\n"))
  cat(paste0("Bootstrap result: ", x$estimator$name, "=", round(x$estimator$value, 3), ", ",
             round(x$estimator$confidence * 100, 0), "% CI [", round(x$estimator$lower, 3), ", ",
             round(x$estimator$upper, 3), "]\n"))
  cat(paste0("SESOI: [", round(x$test$SESOI_lower, 3), ", ", round(x$test$SESOI_upper, 3), "], alpha=", round(x$test$alpha,3)))
  cat("\n\n")

  met <- data.frame(
    Test = x$test$test,
    p.value = x$results$p_value)

  print(met, row.names = FALSE)

  cat("\n")
  cat(paste0("Final inference: ", x$results$inference))

}

# =======================================================
#' S3 method for plotting \code{\link{bootstrap_MET}} results
#' @param x Object of class \code{bmbstats_MET}
#' @param type What test to plot? Options are "inferiority", "non-superiority",
#'    "equivalence", "non-inferiority",  "superiority", "CI". Default is "CI"
#' @param ... Extra arguments. Use \code{plot_control} for plotting options
#' @export
#' @examples
#' mean_MET <- bootstrap_MET(
#'   describe_data(rnorm(10, 100, 20)),
#'   estimator = "mean",
#'   SESOI_lower = 95,
#'   SESOI_upper = 105,
#'   alpha = 0.05
#' )
#' mean_MET
#' plot(mean_MET)
plot.bmbstats_MET <- function(x, type = "CI", ...) {
  rlang::arg_match(type, c(x$test$test, "CI"))

  if (type == "CI") {
    plot_MET_CI(x, ...)
  } else {
    plot_MET_distribution(x, type, ...)
  }

}


# -------------------------------------------------------------------

plot_MET_distribution <- function(x, type, control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  ..x.. <- NULL
  null_hypothesis <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  # extract values from bootstrap_MET_out
  estimator_value <- x$estimator$value
  estimator_name <- x$estimator$name
  MET_p_values <- x$result$p_value
  SESOI_lower <- x$test$SESOI_lower
  SESOI_upper <- x$test$SESOI_upper
  SESOI_lower_distribution <- x$distribution$SESOI_lower_distribution
  SESOI_upper_distribution <- x$distribution$SESOI_upper_distribution

  extreme_colors <- control$effect_colors

  # Plot distributions
  gg <- NULL
  if (type == "inferiority") {
    gg <- ggplot2::ggplot(
      SESOI_lower_distribution,
      ggplot2::aes(
        x = null_hypothesis,
        y = 1
      )
    ) +
      cowplot::theme_cowplot(control$font_size) +
      ggridges::geom_density_ridges_gradient(
        ggplot2::aes(
          fill = factor(
            ifelse(
              ..x.. > estimator_value,
              "Equivalent",
              "Lower"
            ),
            levels = c("Equivalent", "Lower"),
            ordered = TRUE
          )
        ),
        color = NA
      ) +
      ggridges::geom_density_ridges_gradient(
        data = SESOI_upper_distribution,
        ggplot2::aes(
          x = null_hypothesis,
          y = 1,
          fill = "Equivalent"
        ),
        #fill = extreme_colors[1],
        color = NA
      ) +
      ggplot2::ggtitle("Inferiority test") +
      ggplot2::annotate(
        "text",
        x = estimator_value,
        y = 1,
        label = paste(
          "p=",
          round(MET_p_values[1], 2),
          " ",
          sep = ""
        ),
        hjust = "right",
        vjust = "bottom"
      )
  }

  if (type == "non-superiority") {
    gg <- ggplot2::ggplot(
      SESOI_upper_distribution,
      ggplot2::aes(
        x = null_hypothesis,
        y = 1
      )
    ) +
      cowplot::theme_cowplot(control$font_size) +
      ggridges::geom_density_ridges_gradient(
        ggplot2::aes(
          fill = factor(
            ifelse(
              ..x.. > estimator_value,
              "Equivalent",
              "Lower"
            ),
            levels = c("Equivalent", "Lower"),
            ordered = TRUE
          )
        ),
        color = NA
      ) +
      ggridges::geom_density_ridges_gradient(
        data = SESOI_lower_distribution,
        ggplot2::aes(
          x = null_hypothesis,
          y = 1,
          fill = "Equivalent"
        ),
        #fill = extreme_colors[1],
        color = NA
      ) +
      ggplot2::ggtitle("Non-Superiority test") +
      ggplot2::annotate("text",
                        x = estimator_value,
                        y = 1,
                        label = paste(
                          "p=",
                          round(MET_p_values[2], 2),
                          " ",
                          sep = ""
                        ),
                        hjust = "right",
                        vjust = "bottom"
      )
  }

  if (type == "equivalence") {
    gg <- ggplot2::ggplot(
      SESOI_lower_distribution,
      ggplot2::aes(
        x = null_hypothesis,
        y = 1
      )
    ) +
      cowplot::theme_cowplot(control$font_size) +
      ggridges::geom_density_ridges_gradient(
        ggplot2::aes(
          fill = factor(
            ifelse(
              ..x.. < estimator_value,
              "Equivalent",
              "Lower"
            ),
            levels = c("Equivalent", "Lower"),
            ordered = TRUE
          )
        ),
        color = NA
      ) +
      ggridges::geom_density_ridges_gradient(
        data = SESOI_upper_distribution,
        ggplot2::aes(
          x = null_hypothesis,
          y = 1,
          fill = factor(
            ifelse(
              ..x.. > estimator_value,
              "Equivalent",
              "Lower"
            ),
            levels = c("Equivalent", "Lower"),
            ordered = TRUE
          )
        ),
        color = NA
      ) +
      ggplot2::ggtitle("Equivalence test") +
      ggplot2::annotate(
        "text",
        x = estimator_value,
        y = 1,
        label = paste(
          " p=",
          round(MET_p_values[3], 2),
          " ",
          sep = ""
        ),
        hjust = ifelse(
          estimator_value - SESOI_lower > SESOI_upper - estimator_value,
          "right",
          "left"
        ),
        vjust = "bottom"
      )
  }

  if (type == "non-inferiority") {
    gg <- ggplot2::ggplot(
      SESOI_lower_distribution,
      ggplot2::aes(
        x = null_hypothesis,
        y = 1
      )
    ) +
      cowplot::theme_cowplot(control$font_size) +
      ggridges::geom_density_ridges_gradient(
        ggplot2::aes(
          fill = factor(
            ifelse(
              ..x.. < estimator_value,
              "Equivalent",
              "Lower"
            ),
            levels = c("Equivalent", "Lower"),
            ordered = TRUE
          )
        ),
        color = NA
      ) +
      ggridges::geom_density_ridges_gradient(
        data = SESOI_upper_distribution,
        ggplot2::aes(
          x = null_hypothesis,
          y = 1,
          fill = "Equivalent"
        ),
        #fill = extreme_colors[1],
        color = NA
      ) +
      ggplot2::ggtitle("Non-Inferiority test") +
      ggplot2::annotate(
        "text",
        x = estimator_value,
        y = 1,
        label = paste(
          " p=",
          round(MET_p_values[4], 2),
          sep = ""
        ),
        hjust = "left",
        vjust = "bottom"
      )
  }

  if (type == "superiority") {
    gg <- ggplot2::ggplot(
      SESOI_upper_distribution,
      ggplot2::aes(
        x = null_hypothesis,
        y = 1
      )
    ) +
      cowplot::theme_cowplot(control$font_size) +
      ggridges::geom_density_ridges_gradient(
        ggplot2::aes(
          fill = factor(
            ifelse(
              ..x.. < estimator_value,
              "Equivalent",
              "Lower"
            ),
            levels = c("Equivalent", "Lower"),
            ordered = TRUE
          )
        ),
        color = NA
      ) +
      ggridges::geom_density_ridges_gradient(
        data = SESOI_lower_distribution,
        ggplot2::aes(
          x = null_hypothesis,
          y = 1,
          fill = "Equivalent"
        ),
        #fill = extreme_colors[1],
        color = NA
      ) +
      ggplot2::ggtitle("Superiority test") +
      ggplot2::annotate(
        "text",
        x = estimator_value,
        y = 1,
        label = paste(
          " p=",
          round(MET_p_values[5], 2),
          sep = ""
        ),
        hjust = "left",
        vjust = "bottom"
      )
  }

  gg <- gg +
    ggplot2::geom_vline(
      xintercept = SESOI_lower,
      color = "black"
    ) +
    ggplot2::geom_vline(
      xintercept = SESOI_upper,
      color = "black"
    ) +
    ggplot2::geom_vline(
      xintercept = estimator_value,
      color = "black",
      linetype = "dashed"
    ) +
    ggplot2::scale_discrete_manual(
      aesthetics = c("point_color", "fill"),
      values = extreme_colors,
      drop = FALSE,
      limits = c("Equivalent", "Lower")
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +

    ggplot2::xlab(estimator_name)
  return(gg)
}

# --------------------------------------------------
plot_MET_CI <- function(x, control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  y <- NULL
  xmin <- NULL
  xmax <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++


  # extract values from bootstrap_MET_out
  estimator_value <- x$estimator$value
  estimator_upper <- x$estimator$upper
  estimator_lower <- x$estimator$lower
  estimator_name <- x$estimator$name
  SESOI_lower <- x$test$SESOI_lower
  SESOI_upper <- x$test$SESOI_upper
  confidence <- x$estimator$confidence
  alpha <- x$test$alpha

  # Create significance string
  p_value <- x$result$p_value
  p_value <- ifelse(p_value < alpha, "*", "-")
  p_value <- paste(p_value, collapse = "")

  final_inference <- x$result$inference

  draw_df <- data.frame(
    x = estimator_value,
    xmax = estimator_upper,
    xmin = estimator_lower,
    y = 1
  )

  gg <- ggplot2::ggplot(
    draw_df,
    ggplot2::aes(
      x = x,
      y = y
    )
  ) +
    cowplot::theme_cowplot(control$font_size) +
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
      ggplot2::aes(xmin = xmin, xmax = xmax),
      size = control$summary_bar_size,
      color = control$summary_bar_color
    ) +

    ggplot2::theme(legend.position = "none") +
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
        estimator_name,
        " = ",
        round(estimator_value, 2),
        "\n",
        round(confidence * 100, 0),
        "% CI [",
        round(estimator_lower, 2),
        "; ",
        round(estimator_upper, 2),
        "]",
        sep = ""
      ),
      paste(
        "Inference: ",
        final_inference,
        " [",
        p_value,
        "] for alpha=",
        alpha,
        sep = ""
      )
    )

  return(gg)
}
