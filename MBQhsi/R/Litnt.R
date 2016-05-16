#' @title Literature Number of Trees
#'
#' @description Calculates the partial HSI given the number of trees.
#'
#' @param x From Simms (1989): Number of trees with a height between 0 and 5 meters per hectare.
#'
#' @return Returns the relative HSI value
#'
#' @usage NT.Lit(x)
#'
#'
NT.Lit <- function(x){
  s <- dnorm(x, 150, 15) * 37.5994
  return(s)
}
