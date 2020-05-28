
#' RCT Predict
#'
#' \code{RCT_predict} perform additional analysis on \code{model} returned by \code{\link{cv_model}} function
#'     assuming RCT data
#'
#' @inheritParams RCT_analysis
#' @inheritParams basic_arguments
#' @param model Object returned by \code{\link{cv_model}} function
#' @param new_data Data frame
#' @param outcome Character string indicating the outcome column in \code{new_data}
#' @param subject_label Row labels, usually participants names. Default is \code{new_data} row names
#'
#' @return Object of class `bmbstats_RCT_predict`
#' @export
#'
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
RCT_predict <- function(model,
                        new_data,
                        outcome,
                        group,
                        control_label,
                        treatment_label,
                        subject_label = rownames(new_data),
                        na.rm = FALSE) {

 if(class(model)[[1]] != "bmbstats_cv_model") {
   stop("Model parameter must be object returned by cv_model function.", call. = FALSE)
 }
 # SESOI
  SESOI_lower <- func_num(model$SESOI_lower, model$predictors, model$outcome, model$na.rm)
  SESOI_upper <- func_num(model$SESOI_upper, model$predictors, model$outcome, model$na.rm)

  # ----------------------------
 # Residuals analysis per group
 observed <- new_data[[outcome]]
 predicted <- stats::predict(model, new_data)[[1]]
 residual <- predicted - observed

 # Save to DF
 residual_df <- data.frame(
   subject = subject_label,
   group = new_data[[group]],
   observed = observed,
   predicted = predicted,
   residual = residual,
   magnitude = get_magnitude(residual, SESOI_lower, SESOI_upper)
 )

 residual_list <- split(residual_df, residual_df$group)

 residual_summary <- purrr::map2_df(residual_list, names(residual_list), function(group, group_name){
   data.frame(
     group = group_name,
     mean = mean(group$residual, na.rm = na.rm),
     SD = stats::sd(group$residual, na.rm = na.rm)
   )
 })

 model$extra <- list(
   new_data = new_data,
   group = group,
   outcome = outcome,
   control_label = control_label,
   treatment_label = treatment_label,
   SESOI_lower = SESOI_lower,
   SESOI_upper = SESOI_upper,
   residual_df = residual_df,
   residual_summary = residual_summary
 )

 class(model) <- "bmbstats_RCT_predict"

 return(model)

}
