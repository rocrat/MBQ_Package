#' @title Dr.s Sally Gall and Dan Cohan Grass Cover
#'
#' @description Calculates the partial HSI given the grass cover.
#'
#' @param x Grass cover measured as the percent canopy cover of grass. The optimal canopy cover of grass differs between perennial and annual grasses.
#' @param perennial If TRUE then returns the partial HSI for perrennial grass cover, otherwise it returns the partial HSI for annual grass cover (default = TRUE).
#'
#' @return Returns the relative HSI value
#'
#' @usage GC.SGDC(x, perennial = TRUE)
#'
#' @export
#' @name GC.SGDC
#' @author Dominic LaRoche
#'
GC.SGDC <- function(x, perennial = TRUE){
  if(perennial){
    s <- ifelse(x<=.55, 1.818182*x, dbeta(x, 5, 4.090909)/2.351513)
  }else{
    s <- dbeta(x, 2, 3)/1.79
  }
  return(s)
}
