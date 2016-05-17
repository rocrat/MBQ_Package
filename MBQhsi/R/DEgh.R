#' @title Dr. Dave Ellis Grass Height
#'
#' @description Calculates the partial HSI given the grass height.
#'
#' @param x Grass Height measured as the average height of grass on a given home range (in meters).
#'
#' @return Returns the relative HSI value
#'
#' @usage GH.Ellis(x)
#'
#' @export
#' @name GH.Ellis
#' @author Dominic LaRoche
#'
GH.Ellis <- function(x){
  if(any(x > 3)) stop("Grass height should be measured in meters and it is unlikely that grass is over 3 meters in height.")
  s <- dgamma(x, shape = 2, rate = 4)/1.5
  return(s)
}
