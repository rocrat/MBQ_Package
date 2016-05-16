#' @title Literature Grass Diversity
#' @description Calculates the partial HSI given the grass diversity.
#'
#' @param x From Simms (1989): Grass diversity as measured by the mean number of grass species as measured by the Daubenmire method (Daubenmire 1959).
#'
#' @return Returns the relative HSI value
#'
#' @usage GD.Lit(x)
#'
#'
GD.Lit <- function(x){
  s <- pnorm(x, 7, 1.5)
  return(s)
}
