#' @title John Goodwin Forb Cover
#'
#' @description Calculates the partial HSI given the forb cover.
#'
#' @param x Forb cover measured as the average canopy cover of forbs.  Suitability differs in Arizona and Mexico.
#' @param AZ True or False.  Is the habitat in Arizona (TRUE) or Mexico (FALSE).  Defaults to Arizona.
#'
#' @return Returns the relative HSI value
#'
#' @usage FC.Goodwin(x, AZ = TRUE)
#'
#' @export
#' @name FC.Goodwin
#' @author Dominic LaRoche
#'
FC.Goodwin <- function(x, AZ = TRUE){
  if(AZ){
    s <- ifelse(x <= 0.5, (0.2 + (dbeta(x, 5, 19)/6.4)), x - (x^2) - 0.05)
  }else{
    s <- (0.3 + dbeta(x, 10, 18.57))/4.75982
  }
  return(s)
}
