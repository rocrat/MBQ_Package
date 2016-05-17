#' @title Mary Hunnicut Layered Vegetation HSI
#'
#' @description Calculates the partial HSI given the presence of multi-layered vegetation.
#'
#' @param x A 1 or 0 indicating the presence or absence of multi-layered vegetation on each acre.
#'
#' @return Returns the relative HSI value
#'
#' @usage MH.lv(x)
#'
#' @details The presence or absence of multi-layered vegetation on each acre.
#'
#' @export
#' @name MH.lv
#' @author Dominic LaRoche
#'

MH.lv <- function(x){
  s <- ifelse(x == 1, 1, 0)
  return(s)
}
