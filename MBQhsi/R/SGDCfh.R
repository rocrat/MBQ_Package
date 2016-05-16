#' @title Dr.s Sally Gall and Dan Cohan Forb Height
#'
#' @description Calculates the partial HSI given the forb height.
#'
#' @param x Forb height measured as the average height of forbs.  Optimal forb height differs between the spring/summer and the fall/winter.
#' @param summer If TRUE then returns the partial HSI for summer, otherwise it returns the partial HSI for winter (default = TRUE).
#'
#' @return Returns the relative HSI value
#'
#' @usage FH.SGDC(x, summer = TRUE)
#'
#'
FH.SGDC <- function(x, summer = TRUE){
  if(summer){
    s <- 1 + (-pgamma(x, 13, rate = 1))
  }else{
    s <- pgamma(x, 13, rate = 1)
  }
  return(s)
}
