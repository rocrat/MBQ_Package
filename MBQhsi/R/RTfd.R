#' @title Dr. Roy Tomlinson Forb Diversity
#'
#' @description Calculates the partial HSI given the forb diversity.
#'
#' @param x Forb Diversity measured as the total number of forb species  on a typical  home range (10.9 ha).
#'
#' @return Returns the relative HSI value
#'
#' @usage FD.RT(x)
#'
#'
FD.RT <- function(x){
  s <- pgamma(x, 25.5, rate=1)
  return(s)
}
