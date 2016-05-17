#' @title Dr. Dave Ellis Tree Cover
#'
#' @description Calculates the partial HSI given the tree cover.
#'
#' @param x Tree cover measured as the percent canopy cover of trees on a given home range.
#'
#' @return Returns the relative HSI value
#'
#' @usage TC.Ellis(x)
#'
#' @export
#' @name TC.Ellis
#' @author Dominic LaRoche
#'
TC.Ellis <- function(x){
  s <- 1 - pbeta(x, 3, 3.1)
  return(s)
}
