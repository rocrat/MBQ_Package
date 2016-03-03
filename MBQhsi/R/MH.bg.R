#' @title Mary Hunnicut Bare Ground HSI
#' 
#' @description Calculates the HSI for bare ground given a proportion of bare ground 
#' 
#' @param x The proportion of bare ground (in decimal form)
#' 
#' @return Returns the relative HSI value
#' 
#' @usage MH.bg(x)
#' 
MH.bg <- function(x){
  s <- ifelse( x < .2805743, dbeta(x, 3, 4.5)/2, 
               ifelse(x > .4525119, dbeta(x, 3, 4.5)/2, 1))
  return(s)
}