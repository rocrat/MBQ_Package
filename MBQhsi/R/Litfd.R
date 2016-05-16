#' @title Dr. Roy Tomlinson Forb Diversity
#' @description Calculates the partial HSI given the forb diversity.
#'
#' @param x From Simms (1989): Forb diversity as measured by the mean number of forb species as measured by the Daubenmire method (Daubenmire 1959).
#'
#' @return Returns the relative HSI value
#'
#' @usage FD.Lit(x)
#'
#'
FD.Lit <- function(x){
  s <- pnorm(x, 7, 1.5)
  return(s)
}
