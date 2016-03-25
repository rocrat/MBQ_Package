#' @title Mary Hunnicut Forb Diversity HSI
#'
#' @description Calculates the partial HSI given a the forb diversity.
#'
#' @param x The number of forb species per acre, as measured on a line intercept or frame method.
#'
#' @return Returns the relative HSI value
#'
#' @usage MH.fd(x)
#'
#'
MH.fd <- function(x){
  s <- pgamma(x, 20, 2)
  return(s)
}
