#' @title Dr. Dave Ellis Forb Diversity
#'
#' @description Calculates the partial HSI given the forb diversity.
#'
#' @param x Forb Diversity measured as the total number of forb species on a given home range throughout the year.
#'
#' @return Returns the relative HSI value
#'
#' @usage FD.Ellis(x)
#' @export
#' @name FD.Ellis
#' @author Dominic LaRoche
#'
FD.Ellis <- function(x){
  s <- pgamma(x, 22.5, rate = 1)
  return(s)
}
