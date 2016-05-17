#' @title Mary Hunnicut Shrub Cover HSI
#'
#' @description Calculates the partial HSI given a proportion of shrub cover
#'
#' @param x The proportion of shrub cover (in decimal form)
#'
#' @return Returns the relative HSI value
#'
#' @usage MH.sc(x)
#'
#' @details Cover of woody shrubs as measured by aerial photo, or line intercept method, within each acre of ground.
#'
#' @export
#' @name MH.sc
#' @author Dominic LaRoche
#'
MH.sc <- function(x){
  s <- dbeta(x, 1.5, 3.5)/2.108676
  return(s)
}
