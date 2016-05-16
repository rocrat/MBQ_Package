#' @title Dr.s Sally Gall and Dan Cohan Grass Diversity
#'
#' @description Calculates the partial HSI given the grass diversity.
#'
#' @param x Grass diversity measured as the total number of grass species found on a typical home range (10.9 ha).  The optimal number of species differs between perennial and annual grasses.
#' @param perennial If TRUE then returns the partial HSI for perrennial grass cover, otherwise it returns the partial HSI for annual grass diversity (default = TRUE).
#'
#' @return Returns the relative HSI value
#'
#' @usage GD.SGDC(x, perennial = TRUE)
#'
#'
GD.SGDC <- function(x, perennial = TRUE){
  if(perennial){
    s <- pgamma(x, 7, 2.3333)
  }else{
    s <- pgamma(x, 5, 2.5)
  }
  return(s)
}
