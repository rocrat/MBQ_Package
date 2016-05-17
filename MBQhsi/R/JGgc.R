#' @title John Goodwin Grass Cover
#'
#' @description Calculates the partial HSI given the grass cover.
#'
#' @param x Grass cover measured as the average canopy cover of grass.  Suitability differs between Arizona and Mexico.
#' @param AZ True or False.  Is the habitat in Arizona (TRUE) or Mexico (FALSE).  Defaults to Arizona.
#'
#' @return Returns the relative HSI value
#'
#' @usage GC.Goodwin(x, AZ = TRUE)
#'
#' @export
#' @name GC.Goodwin
#' @author Dominic LaRoche
#'
GC.Goodwin <- function(x, AZ = TRUE){
  if(AZ){
    s <- ifelse(x <= 0.28, dbeta(x, 15, 60)/8.8, 0.2)
  }else{
    s <- pgamma(x, shape = 2, rate = 0.444)
  }
  return(s)
}
