#' @title John Goodwin Shrub Cover
#'
#' @description Calculates the partial HSI given the shrub cover.
#'
#' @param x Shrub cover measured as the average canopy cover of shrubs throughout the year. Optimal levels of grass diversity differ in Arizona and Mexico.
#' @param AZ True or False.  Is the habitat in Arizona (TRUE) or Mexico (FALSE).  Defaults to Arizona.
#'
#' @return Returns the relative HSI value
#'
#' @usage SC.Goodwin(x, AZ = TRUE)
#'
#'@details Shrub cover should be distributed in clumps approximately 100 yards apart.
SC.Goodwin <- function(x, AZ = TRUE){
  if(AZ){
    s <- dbeta(x, 5, 20) / 5.155
  }else{
    s <- ifelse(x <= 0.5, dbeta(x, 3, 7.909) / 3.1, x - (x^2) - 0.0157323)
  }
  return(s)
}
