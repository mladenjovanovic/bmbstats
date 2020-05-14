#' Basic arguments
#' @param x Numeric vector
#' @param model Model object
#' @param predictors Data frame
#' @param outcome Vector
#' @param observed Numeric vector
#' @param predicted Numeric vector
#' @param group_a Numeric vector. This group represents baseline/control, Pre-test in the paired design, or "practical" measure
#' @param group_b Numeric vector. This group represents experimental, Post-test in the paired design, or "criterion" measure
#' @param group_a_label Character vector. The name of the \code{group_a}. Default is "Group A"
#' @param group_b_label Character vector. The name of the \code{group_b}. Default is "Group B"
#' @param paired Paired groups? Default is \code{FALSE}
#' @param na.rm Should NAs be removed? Default is \code{FALSE}
#' @param SESOI_lower Lower smallest effect size of interest threshold. Default is 0
#' @param SESOI_upper Upper smallest effect size of interest threshold.  Default is 0
#' @param confidence Default is 0.95
#' @name basic_arguments
NULL
