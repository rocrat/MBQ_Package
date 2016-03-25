#' @title Dr. Dave Ellis Grass Diversity
#'
#' @description Calculates the partial HSI given the grass diversity.
#'
#' @param x Grass Diversity measured as the total number of both annual and perennial grass species on a given home range throughout the year.
#'
#' @return Returns the relative HSI value
#'
#' @usage GD.Ellis(x)
#'
#'
GD.Ellis <- function(x){
  s <- pgamma(x, 22.5, rate = 1)
  return(s)
}
