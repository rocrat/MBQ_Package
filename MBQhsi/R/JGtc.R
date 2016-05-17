#' @title John Goodwin Tree Cover
#'
#' @description Calculates the partial HSI given the tree cover.
#'
#' @param x Tree cover measured as the average canopy cover of trees.  Suitability of tree cover differs between Arizona and Mexico.
#' @param AZ True or False.  Is the habitat in Arizona (TRUE) or Mexico (FALSE).  Defaults to Arizona.
#'
#' @return Returns the relative HSI value
#'
#' @usage TC.Goodwin(x, AZ = TRUE)
#'
#' @export
#' @name TC.Goodwin
#' @author Dominic LaRoche
#'
TC.Goodwin <- function(x, AZ = TRUE){
  if(AZ){
    s <- ifelse(x <= 0.07, 0.2 + dbeta(x, 10, 190)/34, dbeta(x, 10, 190)/34)
  }else{
    s <- ifelse(x <= 0.26, 0.1 + dbeta(x, 15, 70.71)/11, 0.2)
  }
  return(s)
}
