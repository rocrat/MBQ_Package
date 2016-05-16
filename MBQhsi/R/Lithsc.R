#' @title Literature Half-Shrub Cover
#' @description Calculates the partial HSI given the percent half-shrub cover.
#'
#' @param x From Simms (1989):  Percent half-shrub cover measured by the Daubenmire method (Daubenmire 1959).
#'
#' @return Returns the relative HSI value
#'
#' @usage HSC.Lit(x)
#'
#'
HSC.Lit <- function(x){
  s <- 1 - pnorm(x, 1.3, 0.5)
  return(s)
}
