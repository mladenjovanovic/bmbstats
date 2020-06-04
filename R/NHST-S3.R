# =======================================================
#' S3 method for printing \code{\link{bootstrap_NHST}} results
#' @param x Object of class \code{bmbstats_NHST}
#' @param ... Extra arguments. Not used
#' @export
#' @examples
#' mean_NHST <- bootstrap_NHST(
#'   describe_data(rnorm(100)),
#'   "mean"
#' )
#' mean_NHST
#' plot(mean_NHST)
print.bmbstats_NHST <- function(x, ...) {

  cat(paste0("Null-hypothesis significance test for the `", x$estimator$name,"` estimator\n"))
  cat(paste0("Bootstrap result: ", x$estimator$name, "=", round(x$estimator$value, 3), ", ",
             round(x$estimator$confidence * 100, 0), "% CI [", round(x$estimator$lower, 3), ", ",
             round(x$estimator$upper, 3), "]\n"))
  cat(paste0("H0=", round(x$test$null_hypothesis, 3), ", test: ", x$test$test, "\n"))
  cat(paste0("p=", x$results$p_value))
}

# =======================================================
#' S3 method for plotting \code{\link{bootstrap_NHST}} results
#' @param x Object of class \code{bmbstats_NHST}
#' @param ... Extra arguments. Use \code{plot_control} for plotting options, particularly
#'     \code{effect_colors} parameter
#' @export
#' @examples
#' mean_NHST <- bootstrap_NHST(
#'   describe_data(rnorm(100)),
#'   "mean"
#' )
#' mean_NHST
#' plot(mean_NHST)
plot.bmbstats_NHST <- function(x, ...) {
  plot_NHST_null_distribution(x, ...)
}


# ----------------------------------------------------------------------------
plot_NHST_null_distribution <- function(x, control = plot_control()) {
  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  ..x.. <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++


  # Extract data from bootstrap_NHST_out
  estimator_value <- x$estimator$value
  estimator_name <- x$estimator$name
  null_hypothesis <- x$test$null_hypothesis
  null_distribution <- x$distribution$null_distribution
  p_value <- x$result$p_value
  test <- x$test$test

  estimator_max <- max(
    estimator_value, null_hypothesis + (null_hypothesis - estimator_value)
  )

  estimator_min <- min(
    estimator_value, null_hypothesis + (null_hypothesis - estimator_value)
  )

  extreme_colors <- control$effect_colors

  if (test == "two.sided") {
    gg <- ggplot2::ggplot(
      null_distribution,
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
              (..x.. < estimator_max) & (..x.. > estimator_min),
              "Equivalent",
              "Lower"
            ),
            levels = c("Equivalent", "Lower"),
            ordered = TRUE
          )
        ),
        color = NA
      ) +
      ggplot2::annotate(
        "text",
        x = estimator_max,
        y = 1,
        label = paste(
          " p=",
          round(p_value, 3),
          sep = ""
        ),
        hjust = "left",
        vjust = "bottom"
      )
  }

  if (test == "greater") {
    gg <- ggplot2::ggplot(
      null_distribution,
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
              (..x.. < estimator_value),
              "Equivalent",
              "Lower"
            ),
            levels = c("Equivalent", "Lower"),
            ordered = TRUE
          )
        ),
        color = NA
      ) +
      ggplot2::annotate(
        "text",
        x = estimator_value,
        y = 1,
        label = paste(
          " p=",
          round(p_value, 3),
          sep = ""
        ),
        hjust = "left",
        vjust = "bottom"
      )
  }

  if (test == "less") {
    gg <- ggplot2::ggplot(
      null_distribution,
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
              (..x.. > estimator_value),
              "Equivalent",
              "Lower"
            ),
            levels = c("Equivalent", "Lower"),
            ordered = TRUE
          )
        ),
        color = NA
      ) +
      ggplot2::annotate(
        "text",
        x = estimator_value,
        y = 1,
        label = paste(
          "p=",
          round(p_value, 3),
          " ",
          sep = ""
        ),
        hjust = "right",
        vjust = "bottom"
      )
  }

  gg <- gg +
    ggplot2::geom_vline(
      xintercept = null_hypothesis,
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
