#' @title Mary Hunnicut forb cover/ grass cover HSI
#'
#' @description Calculates the partial HSI given a the cover of either green forbs or grass (same function for both).
#'
#' @param x Proportion of green forbs or grass measured as one would in a Daubnenmire frame (per acre).
#'
#' @return Returns the relative HSI value
#'
#' @usage MH.fcgc(x)
#'
#'
MH.fcgc <- function(x){
  s <- ifelse(x < 0.15, dbeta(x, 1.1, 1) * 1.099001, 1)
  return(s)
}
