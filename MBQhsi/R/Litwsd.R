#' @title Literature Woody Stem Density
#'
#' @description Calculates the partial HSI given the Woody Stem Density.
#'
#' @param x From King (1998): The mean number of woody stems >1m tall per 200 square meters.
#'
#' @return Returns the relative HSI value
#'
#' @usage WSD.Lit(x)
#'
#' @export
#' @name WSD.Lit
#' @author Dominic LaRoche
#'
WSD.Lit <- function(x){
  s <- dnorm(x, 47.1, 10.62)*26.62039
  return(s)
}
