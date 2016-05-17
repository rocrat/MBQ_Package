#' @title Dr.s Sally Gall and Dan Cohan Forb Diversity
#'
#' @description Calculates the partial HSI given the forb diversity.
#'
#' @param x Forb Diversity measured as the total number of forb species on a typical  home range (10.9 ha) throughout the year.
#'
#' @return Returns the relative HSI value
#'
#' @usage FD.SGDC(x)
#'
#' @export
#' @name FD.SGDC
#' @author Dominic LaRoche
#'
FD.SGDC <- function(x){
  s <- pgamma(x, 22.5, rate = 1)
  return(s)
}
