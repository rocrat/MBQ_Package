#' @title Dr. Dave Ellis Shrub Diversity
#'
#' @description Calculates the partial HSI given the shrub diversity.
#'
#' @param x Shrub diversity measured as the total number of shrub species on  a given home range throughout the year.
#'
#' @return Returns the relative HSI value
#'
#' @usage SD.Ellis(x)
#'
#'
SD.Ellis <- function(x){
  s <- pgamma(x, 11, rate = 1)
  return(s)
}
