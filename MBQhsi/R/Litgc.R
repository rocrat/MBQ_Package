#' @title Literature Grass Cover
#' @description Calculates the partial HSI given the grass cover.
#'
#' @param x From Simms (1989): Percent aerial grass cover measured as in a Daubenmire plot (Daubenmire 1959).
#'
#' @return Returns the relative HSI value
#'
#' @usage GC.Lit(x)
#'
#'
GC.Lit <- function(x){
  s <- dnorm(x, 50, 12) * 30.07954
  return(s)
}
