#' @title Literature Disc of Vulnerability
#'
#' @description Calculates the partial HSI given the disc of vulnerability.
#'
#' @param x From Guthery et al. (2000): Visual cover measured as a disc of vulnerability (as described in Kopp (1998)) around a random point.
#'
#' @return Returns the relative HSI value
#'
#' @usage DV.Lit(x)
#'
#' @export
#' @name DV.Lit
#' @author Dominic LaRoche
#'
DV.Lit <- function(x){
  s <- 1 - pnorm(x, 15, 4)
  return(s)
}
