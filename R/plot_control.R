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
#' @param identity_line Should identity line be plotted?
#' @param SESOI_band Should SESOI band be plotted?
#' @param SESOI_color Which color should \code{SESOI_band} use?
#' @param SESOI_alpha Transparency for the SESOI band. Default is 0.2
#' @param font_size Overall size of fonts
#' @param legend_position Position of the legend. Default is "none". See \code{\link[ggplot2]{theme}}
#'     for more info
#' @param smooth_method Smoothing method (function) to use, accepts either a character vector, e.g.
#'     "auto", "lm", "glm", "gam", "loess" or a function, e.g. MASS::rlm or mgcv::gam, stats::lm,
#'     or stats::loess. Default is "loess". See \code{\link[ggplot2]{geom_smooth}} for more info
#' @param smooth_se Should standard error be plotted? Defaults is \code{FALSE}
#' @param smooth_color Color of the smooth line
#' @param smooth_fill Color of the smooth confidence area
#' @param smooth_alpha Transparency for the smooth line
#' @param smooth_size  Size of the smooth line
#' @param panel_labels Labels for panels
#' @param panel_labels_size Size of panel labels. Default is 10
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
                         points_fill = "black",
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
                         identity_line = TRUE,
                         SESOI_band = TRUE,
                         SESOI_color = "#4D4D4D6A",
                         SESOI_alpha = 0.2,
                         font_size = 8,
                         legend_position = "none",
                         smooth_method = "lm",
                         smooth_se = FALSE,
                         smooth_color = "#5DA5DA",
                         smooth_fill = "#4D4D4D6A",
                         smooth_alpha = 0.8,
                         smooth_size = 1,
                         panel_labels = c("A", "B", "C", "D", "E", "F"),
                         panel_labels_size = 10,
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
    identity_line = identity_line,
    SESOI_band = SESOI_band,
    SESOI_color = SESOI_color,
    SESOI_alpha = SESOI_alpha,
    font_size = font_size,
    legend_position = legend_position,
    smooth_method = smooth_method,
    smooth_se = smooth_se,
    smooth_color = smooth_color,
    smooth_fill = smooth_fill,
    smooth_alpha = smooth_alpha,
    smooth_size = smooth_size,
    panel_labels = panel_labels,
    panel_labels_size = panel_labels_size,
    coord_flip = coord_flip
  )
}
