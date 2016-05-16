#' @title Dr. Roy Tomlinson Bare Ground
#'
#' @description Calculates the partial HSI given the bare ground.
#'
#' @param x Bare Ground measured as the proportion of surface area not occupied by stems or other obstructions on a typical  home range (10.9 ha).
#'
#' @return Returns the relative HSI value
#'
#' @usage BG.RT(x)
#'
#'
BG.RT <- function(x){
  s <- dbeta(x, 9, 27)/5.571042
  return(s)
}
