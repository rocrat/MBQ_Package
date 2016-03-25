#' @title Mary Hunnicut Layered Vegetation HSI
#'
#' @description Calculates the partial HSI given the presence of an ecotone.
#'
#' @param x A 1 or 0 indicating the presence or absence of an ecotone.
#'
#' @return Returns the relative HSI value
#'
#' @usage MH.eh(x)
#' 
#' @details The presence or absence of an ecotone between drainage and upland grass habitat on each acre.
#'
MH.eh <- function(x){
  s <- ifelse(x == 1, 1, 0)
  return(s)
}
