#' @title Literature Cone of Vulnerability
#'
#' @description Calculates the partial HSI given the cone of vulnerability.
#'
#' @param x From Guthery et al.  (2000):   Visual cover measured as a cone of vulnerability (as described in Kopp (1998)) measured in millions of cubic meters around a random point.
#' @param AZ If TRUE, the partial HSI for Arizona is returned.  Otherwise, the partial HSO for Mexico is returned.
#'
#' @return Returns the relative HSI value
#'
#' @usage CV.Lit(x, AZ = TRUE)
#'
#'
CV.Lit <- function(x, AZ = TRUE){
  if(AZ){
    s <- 1 - pnorm(x, 0.75, 0.15)
  }else{
    s <- 1 - pnorm(x, 1.25, 0.3)
  }
  return(s)
}
