#' @title Dr.s Sally Gall and Dan Cohan Tree Cover
#'
#' @description Calculates the partial HSI given the tree cover.
#'
#' @param x Tree cover measured as the average canopy cover of trees.  The optimal value of tree cover differs between the uplands and arroyos.
#' @param uplands If TRUE then returns the partial HSI for upland tree cover, otherwise it returns the partial HSI for arroyo cover (default = TRUE).
#'
#' @return Returns the relative HSI value
#'
#' @usage TC.SGDC(x, uplands = TRUE)
#'
#' @export
#' @name TC.SGDC
#' @author Dominic LaRoche
#'
TC.SGDC <- function(x, uplands = TRUE){
  if(uplands){
    s <- dnorm(x, 0.05, 0.04)/9.973557
  }else{
    s <- dnorm(x, 0.3, 0.18)/2.216346
  }
  return(s)
}
