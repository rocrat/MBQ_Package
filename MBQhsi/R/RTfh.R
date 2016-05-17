#' @title Dr. Roy Tomlinson Forb Height
#'
#' @description Calculates the partial HSI given the forb height.
#'
#' @param x Forb Height measured as the average height of forbs on a typical  home range (10.9 ha).
#'
#' @return Returns the relative HSI value
#'
#' @usage FH.RT(x)
#'
#' @export
#' @name FH.RT
#' @author Dominic LaRoche
#'
FH.RT <- function(x){
  s <- dgamma(x/100, 4, 8)/1.8
  return(s)
}
