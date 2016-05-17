#' @title Dr.s Sally Gall and Dan Cohan Bare Ground
#'
#' @description Calculates the partial HSI given the bare ground.
#'
#' @param x Bare ground measured as the average canopy cover of bare ground.  Bare ground should be in the form of a matrix interspersed with other canopy components.
#'
#' @return Returns the relative HSI value
#'
#' @usage BG.SGDC(x)
#'
#' @export
#' @name BG.SGDC
#' @author Dominic LaRoche
#'
BG.SGDC <- function(x){
  s <- dbeta(x, 3, 9)/3.322
  return(s)
}
