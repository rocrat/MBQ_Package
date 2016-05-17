#' @title Mary Hunnicut Tree Cover HSI
#'
#' @description Calculates the partial HSI given the tree and shrub diversity.
#'
#' @param x Canopy Cover of trees as measured by a densitometer or aerial photo over each acre of ground.
#'
#' @return Returns the relative HSI value
#'
#' @usage MH.tc(x)
#'
#' @export
#' @name MH.tc
#' @author Dominic LaRoche
#'
MH.tc <- function(x){
  s <- dbeta(x, 1.3, 7.366667) / 4.501841
  return(s)
}
