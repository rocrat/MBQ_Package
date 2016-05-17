#' @title Dr. Dave Ellis Grass Basal Area
#'
#' @description Calculates the partial HSI given the grass basal area.
#'
#' @param x Grass basal area measured as the average area occupied by stems of grass on a given home range.
#'
#' @return Returns the relative HSI value
#'
#' @usage GCB.Ellis(x)
#'
#' @export
#' @name GCB.Ellis
#' @author Dominic LaRoche
#'
GCB.Ellis <- function(x){
  s <- dbeta(x, 5, 20)/5.155
  return(s)
}
