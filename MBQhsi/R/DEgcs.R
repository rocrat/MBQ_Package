#' @title Dr. Dave Ellis Grass Side Cover
#'
#' @description Calculates the partial HSI given the grass side cover.
#'
#' @param x Grass Cover from the side measured as the average amount of distance (in meters) until complete visual obstruction on a given home range.
#'
#' @return Returns the relative HSI value
#'
#' @usage GCS.Ellis(x)
#' @export
#' @name GCS.Ellis
#' @author Dominic LaRoche
#'
#'
GCS.Ellis <- function(x){
  s <- 7 * dgamma(x, shape = 2.5, rate = 0.4)
  return(s)
}
