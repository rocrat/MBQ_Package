#' @title John Goodwin Forb Diversity
#'
#' @description Calculates the partial HSI given the forb diversity.
#'
#' @param x Forb Diversity measured as the total number of forb species found in reasonable abundance on a given home range throughout the year.
#'
#' @return Returns the relative HSI value
#'
#' @usage FD.Goodwin(x)
#'
#'
FD.Goodwin <- function(x){
  s <- ifelse(x <= 4.9, 0.2 + (x/5.5)^2,
              ifelse(x > 15, 1 + (-pgamma(x, shape = 64, rate = 2.35)), (x/x)))
  return(s)
}
