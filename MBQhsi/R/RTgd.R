#' @title Dr. Roy Tomlinson
#' @description Calculates the partial HSI given the grass diversity.
#'
#' @param x Grass Diversity measured as the total number of both annual and perennial grass species  on a typical  home range (10.9 ha).
#'
#' @return Returns the relative HSI value
#'
#' @usage GD.RT(x)
#'
#'
GD.RT <- function(x){
  s <- pgamma(x, 24, rate = 1)
  return(s)
}
