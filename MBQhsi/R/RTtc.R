#' @title Dr. Roy Tomlinson Tree Cover
#'
#' @description Calculates the partial HSI given the tree  and shrub cover.
#'
#' @param x Tree and shrub cover measured as the percent canopy cover of trees on a typical  home range (10.9 ha).  Optimal tree and shrub cover differs between summer and winter. Brush piles can be incorporated to improve suitability.
#' @param summer If TRUE then returns the partial HSI for summer, otherwise it returns the partial HSI for winter (default = TRUE).
#'
#' @return Returns the relative HSI value
#'
#' @usage TC.RT(x, summer = TRUE)
#'
#' @export
#' @name TC.RT
#' @author Dominic LaRoche
#'
TC.RT <- function(x, summer = TRUE){
  if(summer){
    s <- dbeta(x, 8, 58.6667)/10.35954
  }else{
    s <- dbeta(x, 9, 9)/3.34
  }
  return(s)
}
