#' @title Dr. Dave Ellis Shrub Height
#'
#' @description Calculates the partial HSI given the shrub height.
#'
#' @param x Shrub Height measured as the average height of grass on a given home range (in meters).
#'
#' @return Returns the relative HSI value
#'
#' @usage SH.Ellis(x)
#'
#' @export
#' @name SH.Ellis
#' @author Dominic LaRoche
#'
SH.Ellis <- function(x){
  s <- dnorm(x, 1, 0.43)
  return(s)
}
