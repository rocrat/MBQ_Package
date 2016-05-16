#' @title Dr.s Sally Gall and Dan Cohan Forb Cover
#'
#' @description Calculates the partial HSI given the forb cover.
#'
#' @param x Forb Cover measured as the average total canopy cover of forbs on a on a typical home range (10.9 ha).  Suitability of forb cover differs in winter and summer.
#' @param summer If TRUE then returns the partial HSI for summer, otherwise it returns the partial HSI for winter (default = TRUE).
#'
#' @return Returns the relative HSI value
#'
#' @usage FC.RT(x, summer = TRUE)
#'
#'
FC.RT <- function(x, summer = TRUE){
  if(summer){
    s <- dbeta(x, 9, 21)/4.784
  }else{
    s <- dbeta(x, 14, 9)/3.894
  }
  return(s)
}
