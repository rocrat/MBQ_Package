#' @title John Goodwin Grass Diversity
#'
#' @description Calculates the partial HSI given the grass diversity.
#'
#' @param x Grass Diversity measured as the total number of both annual and perennial grass species found in reasonable abundance on a given home range throughout the year.  Optimal levels of grass diversity differ in Arizona and Mexico.
#' @param AZ True or False.  Is the habitat in Arizona (TRUE) or Mexico (FALSE).  Defaults to Arizona.
#'
#' @return Returns the relative HSI value
#'
#' @usage GD.Goodwin(x, AZ = TRUE)
#'
#'
GD.Goodwin <- function(x, AZ = TRUE){
  if(AZ){
    s <- pgamma(x, shape = 2, rate = 0.444)
  }else{
    s <- pgamma(x, shape = 5, rate = 0.476)
  }
  return(s)
}
