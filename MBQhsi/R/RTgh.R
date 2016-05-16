#' @title Dr. Roy Tomlinson Grass Height
#'
#' @description Calculates the partial HSI given the grass height.
#'
#' @param x Grass Height measured as the average height of grass  on a typical  home range (10.9 ha).

#' @return Returns the relative HSI value
#'
#' @usage GH.RT(x)
#'
#'
GH.RT <- function(x){
  s <- dgamma(x, 4, 8)/1.8
  return(s)
}
