#' @title Dr. Roy Tomlinson Grass Cover
#'
#' @description Calculates the partial HSI given the grass cover.
#'
#' @param x Grass Canopy Cover measured from above the grass canopy as the amount of ground covered by grass foliage  on a typical  home range (10.9 ha)
#'
#' @return Returns the relative HSI value
#'
#' @usage GC.RT(x)
#'
#' @export
#' @name GC.RT
#' @author Dominic LaRoche
#'
GC.RT <- function(x){
  s <- dbeta(x, 9, 9)/3.34
  return(s)
}
