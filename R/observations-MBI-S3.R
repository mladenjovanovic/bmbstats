# =======================================================
#' S3 method for printing \code{\link{observations_MBI}} results
#' @param x Object of class \code{bmbstats_observations_MBI}
#' @param ... Extra arguments. Not used
#' @export
#' @examples
#' data("bench_press_data")
#' obs_MBI <- observations_MBI(
#'   bench_press_data$Change,
#'   SESOI_lower = -5,
#'   SESOI_upper = 5,
#'   measurement_error = 2.5,
#'   confidence = 0.9
#' )
#'
#' obs_MBI
#' plot(obs_MBI)
print.bmbstats_observations_MBI <- function(x, ...) {
  df <- do.call(data.frame, x)

  # rownames(df) <- df$observations_label

  print(df[c(2, 7, 8, 9, 10)], row.names = FALSE)
}

# =======================================================
#' S3 method for plotting \code{\link{observations_MBI}} results
#' @param x Object of class \code{bmbstats_observations_MBI}
#' @param ... Extra arguments. Use \code{plot_control} for plotting options and
#'     \code{true_observations} for plotting true or supplementary observations
#' @export
#' @examples
#' data("bench_press_data")
#' obs_MBI <- observations_MBI(
#'   bench_press_data$Change,
#'   SESOI_lower = -5,
#'   SESOI_upper = 5,
#'   measurement_error = 2.5,
#'   confidence = 0.9
#' )
#'
#' obs_MBI
#' plot(obs_MBI)
plot.bmbstats_observations_MBI <- function(x, ...) {
  plot_bmbstats_observations_MET(x, ...)
}
