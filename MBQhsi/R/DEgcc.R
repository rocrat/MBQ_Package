#' @title Dr. Dave Ellis Grass Canopy Cover
#'
#' @description Calculates the partial HSI given the grass canopy cover.
#'
#' @param x Grass Canopy Cover measured from above the grass canopy as the amount of ground covered by grass foliage on a given home range.
#'
#' @return Returns the relative HSI value
#'
#' @usage GCC.Ellis(x)
#'
#'
GCC.Ellis <- function(x){
  s <- pbeta(x, 3, 3.1)
  return(s)
}
