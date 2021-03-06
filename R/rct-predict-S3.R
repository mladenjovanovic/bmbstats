# =======================================================
#' S3 method for predicting \code{\link{RCT_predict}} results
#' @param object Object of class "bmbstats_RCT_predict"
#' @param newdata Data used for prediction
#' @param ... Extra arguments forwarded to \code{cv_model} predict
#' @export
#' @examples
#' data("vertical_jump_data")
#'
#' m1 <- cv_model(
#'   `Post-test` ~ `Pre-test` + Group,
#'   vertical_jump_data,
#'   control = model_control(
#'     cv_repeats = 10,
#'     cv_folds = 3,
#'     cv_strata = vertical_jump_data$Group
#'   )
#' )
#'
#' m1_rct <- RCT_predict(
#'   m1,
#'   new_data = vertical_jump_data,
#'   outcome = "Post-test",
#'   group = "Group",
#'   treatment_label = "Treatment",
#'   control_label = "Control"
#' )
#'
#' predict(m1_rct, vertical_jump_data)
predict.bmbstats_RCT_predict <- function(object, newdata, ...) {
  class(object) <- "bmbstats_cv_model"

  stats::predict(object, newdata, ...)
}

# =======================================================
#' S3 method for printing \code{\link{RCT_predict}} results
#' @param x Object of class \code{bmbstats_RCT_predict}
#' @param ... Extra arguments. Not used
#' @export
#' @examples
#' data("vertical_jump_data")
#'
#' m1 <- cv_model(
#'   `Post-test` ~ `Pre-test` + Group,
#'   vertical_jump_data,
#'   control = model_control(
#'     cv_repeats = 10,
#'     cv_folds = 3,
#'     cv_strata = vertical_jump_data$Group
#'   )
#' )
#'
#' m1_rct <- RCT_predict(
#'   m1,
#'   new_data = vertical_jump_data,
#'   outcome = "Post-test",
#'   group = "Group",
#'   treatment_label = "Treatment",
#'   control_label = "Control"
#' )
#'
#' m1_rct
#' plot(m1_rct)
print.bmbstats_RCT_predict <- function(x, ...) {
  cat(
    "Training data consists of", ncol(x$predictors), ifelse(ncol(x$predictors) == 1, "predictor", "predictors"),
    "and", nrow(x$predictors), "observations."
  )


  if (is.list(x$cross_validation)) {
    cat(
      "\nCross-Validation of the model was performed using", x$control$cv_repeats,
      ifelse(x$control$cv_repeats == 1, "repeat", "repeats"), "of",
      x$control$cv_folds, "folds.\n"
    )

    cat("\nModel performance:\n\n")
    print(x$cross_validation$performance$summary$overall, row.names = FALSE)
  } else {
    cat("\nCross-Validation of the model was not performed.")
  }

  cat("\nIndividual model results:\n\n")
  print(x$extra$results, row.names = FALSE)

  cat("\nSummary of residuals per RCT group:\n\n")
  print(x$extra$residual_summary, row.names = FALSE)

  cat("\nSummary of counterfactual effects of RCT group:\n\n")
  print(x$extra$counterfactual_summary, row.names = FALSE)

  cat("\nTreatment effect summary\n\n")
  cat("Average Treatment effect: ", x$extra$average_treatment_effect)
  cat("\nVariable Treatment effect: ", x$extra$variable_treatment_effect)
  cat("\nRandom Treatment effect: ", x$extra$random_treatment_effect)
}

#' S3 method for plotting \code{\link{RCT_predict}} results
#'
#'
#' @param x Object of class \code{bmbstats_RCT_predict}
#' @param type Type of plot. Options are "residuals", "prediction", "bias-variance", "pdp+ice", "counterfactual", and
#'   "ice". Default is "residuals"
#' @param ... Extra arguments. Use \code{control} argument and \code{\link{plot_control}} function to control plotting style.
#'    \code{confidence} for setting confidence level, \code{metric} for selecting performance metric (default
#'    is "RMSE") and \code{metric_CV} for selecting source of CV metrics (defaults is "testing.pooled"). For "pdp+ice",
#'    use \code{predictor} to select predictor for plotting (default is \code{group} parameter from \code{\link{RCT_predict}}).
#' @export
#' @examples
#' data("vertical_jump_data")
#'
#' m1 <- cv_model(
#'   `Post-test` ~ `Pre-test` + Group,
#'   vertical_jump_data,
#'   control = model_control(
#'     cv_repeats = 10,
#'     cv_folds = 3,
#'     cv_strata = vertical_jump_data$Group
#'   )
#' )
#'
#' m1_rct <- RCT_predict(
#'   m1,
#'   new_data = vertical_jump_data,
#'   outcome = "Post-test",
#'   group = "Group",
#'   treatment_label = "Treatment",
#'   control_label = "Control"
#' )
#'
#' m1_rct
#' plot(m1_rct)
plot.bmbstats_RCT_predict <- function(x, type = "residuals", ...) {
  rlang::arg_match(type, c(
    "residuals",
    "prediction",
    "bias-variance",
    "pdp+ice",
    "counterfactual",
    "ice"
  ))

  gg <- list(NULL)

  # Residuals
  if (type == "residuals") {
    gg <- RCT_predict_plot_residuals(x, ...)
  }

  # Residuals
  if (type == "prediction") {
    gg <- RCT_predict_plot_prediction(x, ...)
  }


  if (type == "bias-variance") {
    gg <- RCT_predict_plot_bias_variance(x, ...)
  }

  if (type == "pdp+ice") {
    gg <- RCT_predict_plot_pdp_ice(x, ...)
  }

  if (type == "counterfactual") {
    gg <- RCT_predict_plot_counterfactual(x, ...)
  }

  if (type == "ice") {
    gg <- RCT_predict_plot_ice(x, ...)
  }

  return(gg)
}

# -------------------------------------------------------------------
RCT_predict_plot_residuals <- function(x, confidence = 0.95, control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  group <- NULL
  predicted <- NULL
  residual <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  plot_data <- x$extra$results
  plot_data$group <- factor(plot_data$group)

  n_observations <- nrow(plot_data)
  mean_difference <- mean(plot_data$residual)
  LOA <- stats::sd(plot_data$residual) * stats::qt(
    1 - ((1 - confidence) / 2),
    df = n_observations - 1
  )
  LOA_lower <- mean_difference - LOA
  LOA_upper <- mean_difference + LOA

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(y = residual, x = predicted, color = group, fill = group)
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = x$extra$SESOI_lower,
      ymax = x$extra$SESOI_upper,
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
      shape = control$points_shape
      # color = control$points_color,
      # fill = control$points_fill
    ) +
    # Smooth
    ggplot2::geom_smooth(
      ggplot2::aes(group = 1),
      formula = y ~ x,
      method = control$smooth_method,
      level = confidence,
      se = control$smooth_se,
      size = control$smooth_size,
      alpha = control$smooth_alpha,
      fill = control$smooth_fill,
      color = control$smooth_color
    ) +
    ggplot2::labs(x = "Fitted", y = "Residuals") +
    ggplot2::scale_color_manual(values = control$group_colors) +
    ggplot2::scale_fill_manual(values = control$group_colors) +
    ggplot2::theme(
      legend.position = control$legend_position
    )
}

# -------------------------------------------------------------------
RCT_predict_plot_prediction <- function(x, metric = "RMSE", metric_cv = "testing.pooled", control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  subject <- NULL
  predicted <- NULL
  observed <- NULL
  magnitude <- NULL
  predicted_upper <- NULL
  predicted_lower <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  plot_data <- x$extra$results

  if (control$sort) {
    plot_data$subject <- factor(
      plot_data$subject,
      levels = plot_data$subject[order(plot_data$observed)]
    )
  }

  # Get metrics
  df_metrics <- x$cross_validation$performance$summary$overall
  plot_data$prediction_metric <- df_metrics[df_metrics$metric == metric, metric_cv]

  plot_data$predicted_lower <- plot_data$predicted - plot_data$prediction_metric
  plot_data$predicted_upper <- plot_data$predicted + plot_data$prediction_metric

  ggplot2::ggplot(plot_data, ggplot2::aes(y = subject)) +
    cowplot::theme_cowplot(control$font_size) +
    ggstance::geom_linerangeh(ggplot2::aes(
      xmax = predicted,
      xmin = observed,
      color = magnitude
    ),
    size = control$bar_size, alpha = control$bar_alpha
    ) +
    ggstance::geom_linerangeh(ggplot2::aes(
      xmax = predicted_upper,
      xmin = predicted_lower
    ),
    size = control$summary_bar_size,
    color = control$summary_bar_color,
    alpha = control$summary_bar_alpha
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = predicted),
      shape = "|",
      size = control$points_size,
      alpha = control$points_alpha
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = observed),
      shape = control$points_shape,
      size = control$points_size,
      alpha = control$points_alpha
    ) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(x$extra$outcome) +
    ggplot2::facet_wrap(group ~ ., scales = "free_y") +
    ggplot2::scale_discrete_manual(
      aesthetics = c("color", "fill"),
      values = control$effect_colors,
      drop = FALSE,
      limits = levels(plot_data$magnitude)
    ) +
    ggplot2::labs(color = "Residual") +
    ggplot2::theme(legend.position = control$legend_position)
}


# -------------------------------------------------------------------
RCT_predict_plot_bias_variance <- function(x, control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  id <- NULL
  group <- NULL
  value <- NULL
  key <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  bias_variance_df <- x$cross_validation$bias_variance
  bias_variance_df$id <- x$extra$results$subject[bias_variance_df$index]
  bias_variance_df$group <- x$extra$results$group

  if (control$sort) {
    bias_variance_df$id <- factor(
      bias_variance_df$id,
      levels = bias_variance_df$id[order(bias_variance_df$MSE)]
    )
  }
  bias_variance_df <- bias_variance_df[, c("id", "group", "bias_squared", "variance")]

  bias_variance_df <- tidyr::gather(bias_variance_df, "key", "value", -id, -group)

  bias_variance_df$key <- factor(
    bias_variance_df$key,
    levels = c("bias_squared", "variance"),
    labels = c("Bias.Squared", "Variance")
  )

  ggplot2::ggplot(
    bias_variance_df,
    ggplot2::aes(x = id, y = value, fill = key)
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = control$group_colors) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = control$legend_position
    ) +
    ggplot2::facet_wrap(group ~ ., scales = "free_y")
}

# -------------------------------------------------------------------
RCT_predict_plot_pdp_ice <- function(x, predictor = NULL, control = plot_control()) {
  if (is.null(predictor)) predictor <- x$extra$group

  pdp_plot <- pdp::partial(
    x,
    train = x$extra$new_data,
    pred.var = predictor,
    plot = TRUE,
    rug = FALSE,
    ice = TRUE,
    plot.engine = "ggplot2",
    alpha = control$line_alpha,
    pred.fun = function(object, newdata) {
      stats::predict(object, newdata)
    }
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::xlab(predictor) +
    ggplot2::ylab(x$extra$outcome)
}


# -------------------------------------------------------------------
RCT_predict_plot_counterfactual <- function(x, control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  subject <- NULL
  predicted <- NULL
  counterfactual <- NULL
  observed <- NULL
  pITE_magnitude <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  plot_df <- x$extra$results

  if (control$sort) {
    plot_df$subject <- factor(
      plot_df$subject,
      levels = plot_df$subject[order(plot_df$predicted)]
    )
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(y = subject)
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = predicted,
        xend = counterfactual,
        y = subject,
        yend = subject,
        color = pITE_magnitude
      ),
      arrow = ggplot2::arrow(
        length = ggplot2::unit(0.2, "cm"),
        type = "closed"
      ),
      size = control$line_size,
      alpha = control$line_alpha
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = predicted),
      size = control$points_size,
      color = control$points_color,
      alpha = control$points_alpha
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = observed),
      shape = "|",
      size = control$points_size,
      color = control$points_color,
      alpha = control$points_alpha
    ) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(paste("Counterfactual", x$extra$outcome)) +
    ggplot2::facet_wrap(group ~ ., scales = "free_y") +
    ggplot2::scale_color_manual(values = control$effect_colors) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = control$legend_position
    )
}

# -------------------------------------------------------------------
RCT_predict_plot_ice <- function(x, predictor = NULL, control = plot_control()) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  group <- NULL
  subject <- NULL
  yhat <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++
  if (is.null(predictor)) predictor <- x$extra$group

  plot_df <- x$extra$results

  pdp_df <- pdp::partial(
    x,
    train = x$extra$new_data,
    pred.var = predictor,
    plot = FALSE,
    rug = FALSE,
    pred.fun = function(object, newdata) {
      stats::predict(object, newdata)
    }
  )

  colnames(pdp_df)[1] <- "predictor"

  pdp_df$subject <- plot_df$subject[pdp_df$yhat.id]
  pdp_df$group <- plot_df$group[pdp_df$yhat.id]

  ggplot2::ggplot(
    pdp_df,
    ggplot2::aes(x = predictor, y = yhat, group = subject, color = group)
  ) +
    cowplot::theme_cowplot(control$font_size) +
    ggplot2::geom_line(alpha = control$line_alpha, size = control$line_size) +
    ggplot2::ylab(paste("Predicted", x$extra$outcome)) +
    ggplot2::xlab(predictor) +
    ggplot2::scale_color_manual(values = control$group_colors) +
    ggplot2::facet_wrap(~subject) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = control$legend_position
    )
}
