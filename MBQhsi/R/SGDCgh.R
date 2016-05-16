#' @title Dr.s Sally Gall and Dan Cohan Grass Height
#'
#' @description Calculates the partial HSI given the grass height.
#'
#' @param x Grass height measured as the average height of grass on a typical home range (10.9 ha).  The two experts differed on their assessment of optimal grass height.
#' @param expert 1 or 2. If 1 the "expert 1" model is used (see LaRoche and Conway 2013), if 2 the "expert 2" model is used
#'
#' @return Returns the relative HSI value
#'
#' @usage GH.SGDC(x, expert = TRUE)
#'
#'
GH.SGDC <- function(x, expert = TRUE){
  if(expert == 1){
    s <- dnorm(x, 1.07, 0.45)*1.127983
  }else{
    s <- dnorm(x, 1.37, 0.4)*1.002
  }
  return(s)
}
