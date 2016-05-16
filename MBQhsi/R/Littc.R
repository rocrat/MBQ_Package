#' @title Literature Tree Cover
#'
#' @description Calculates the partial HSI given the tree  and shrub cover.
#'
#' @param x From Simms (1989): Percent tree and shrub cover as measured by a line intercept method (Canfield 1941).
#'
#' @return Returns the relative HSI value
#'
#' @usage TC.Lit(x)
#'
#'
TC.Lit <- function(x){
  s <- dnorm(x, 10, 3) * 7.519885
  return(s)
}
