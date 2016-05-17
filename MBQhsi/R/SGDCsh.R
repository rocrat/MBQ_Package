#' @title Dr.s Sally Gall and Dan Cohan Shrub Height
#'
#' @description Calculates the partial HSI given the shrub height.
#'
#' @param x Shrub height measured as the average height of shrubs.
#'
#' @return Returns the relative HSI value
#'
#' @usage SH.SGDC(x)
#'
#' @export
#' @name SH.SGDC
#' @author Dominic LaRoche
#'
SH.SGDC <- function(x){
  s <- dgamma(x, 10, 2.5)*3.05
  return(s)
}
