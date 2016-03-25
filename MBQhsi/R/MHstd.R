#' @title Mary Hunnicut Shrub and Tree Diversity
#'
#' @description Calculates the partial HSI given the tree and shrub diversity.
#'
#' @param x The number of shrub and tree species, per acre, as counted on a line transect or quadrant.
#'
#' @return Returns the relative HSI value
#'
#' @usage MH.std(x)
#'
#'
MH.std <- function(x){
  s <- pgamma(x, 2, 0.8)
  return(s)
}
