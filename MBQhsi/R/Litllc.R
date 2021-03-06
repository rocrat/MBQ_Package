#' @title Literature Lehmann's lovegrass
#' @description Calculates the partial HSI given the percent cover of Lehmann's lovegrass.
#'
#' @param x From Simms (1989): Percent cover of Lehmann’s lovegrass measured by the Daubenmire method (Daubenmire 1959).
#'
#' @return Returns the relative HSI value
#'
#' @usage LLG.Lit(x)
#'
#' @export
#' @name LLG.Lit
#' @author Dominic LaRoche
#'
LLG.Lit <- function(x){
  s <- 1 - pnorm(x, 1.3, 0.5)
  return(s)
}
