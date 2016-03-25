#' @title Mary Hunnicut Grass Diversity HSI
#'
#' @description Calculates the partial HSI given a the grass diversity.
#'
#' @param x The number of native grass species per acre as counted on a line intercept.
#'
#' @return Returns the relative HSI value
#'
#' @usage MH.gd(x)
#'
#'
MH.gd <- function(x){
  s <- pgamma(x, 10, 1.428571)
  return(s)
}
