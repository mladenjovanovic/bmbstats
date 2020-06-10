#' Basic arguments
#' @param x Numeric vector
#' @param model Model object
#' @param predictors Data frame
#' @param predictor Numeric vector
#' @param outcome Vector
#' @param pre Numeric vector
#' @param post Numeric vector
#' @param observed Numeric vector
#' @param predicted Numeric vector
#' @param group_a Numeric vector. This group represents baseline/control, observed variable, Pre-test in the paired design, or "practical" measure
#' @param group_b Numeric vector. This group represents experimental, predicted variable, Post-test in the paired design, or "criterion" measure
#' @param group_a_label Character vector. The name of the \code{group_a}. Default is "Group A"
#' @param group_b_label Character vector. The name of the \code{group_b}. Default is "Group B"
#' @param fitted_label Character vector. The label to be used for fitted. Default is "Fitted"
#' @param residuals_label Character vector. The label to be used for residuals. Default is "Residuals"
#' @param paired Paired groups? Default is \code{FALSE}
#' @param na.rm Should NAs be removed? Default is \code{FALSE}
#' @param SESOI_lower Lower smallest effect size of interest threshold
#' @param SESOI_upper Upper smallest effect size of interest threshold
#' @param confidence Default is 0.95
#' @param alpha Numeric value for the alpha (Type I error). Default is 0.05
#' @name basic_arguments
NULL
