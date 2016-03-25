#' @title Dr. Dave Ellis Forb Height
#'
#' @description Calculates the partial HSI given the forb height.
#'
#' @param x Forb Height measured as the average height of Forbs on a given home range in centimeters.
#'
#' @return Returns the relative HSI value
#'
#' @usage FH.Ellis(x)
#'
#'
FH.Ellis <- function(x){
  s <- ifelse(x < 33, ((x^2)/1089),
              ifelse(x > 80, 1 + (-pgamma(x, 115, rate = 1)), (x/x)))
  return(s)
}
