#' @title Literature Basal Grass Cover
#' @description Calculates the partial HSI given the basal grass cover.
#'
#' @param x From Simms (1989): Percent basal grass cover measured as in a Daubenmire plot (Daubenmire 1959).
#'
#' @return Returns the relative HSI value
#'
#' @usage GCB.Lit(x)
#'
#' @export
#' @name GCB.Lit
#' @author Dominic LaRoche
#'
GCB.Lit <- function(x){
  s <- dnorm(x, 50, 12) * 30.07954
  return(s)
}
