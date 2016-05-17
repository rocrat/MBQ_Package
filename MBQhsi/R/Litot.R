#' @title Literature Operative Temperature
#' @description Calculates the partial HSI given the percent bare ground.
#'
#' @param x From Guthery et al. (2001): Operative temperature measured as described in Forrester et al. (1998).
#'
#' @return Returns the relative HSI value
#'
#' @usage OT.Lit(x)
#'
#' @export
#' @name OT.Lit
#' @author Dominic LaRoche
#'
OT.Lit <- function(x){
  s <- s <- 1 - pnorm(x, 25, 3)
  return(s)
}
