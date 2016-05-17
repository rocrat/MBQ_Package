#' @title Literature Number of Forbs
#'
#' @description Calculates the partial HSI given the number of forbs.
#'
#' @param x From King (1998): The number of forbs counted per 1000 square centimeters (as in a Daubenmire frame (Daubenmire 1959)).
#'
#' @return Returns the relative HSI value
#'
#' @usage NF.Lit(x)
#'
#' @export
#' @name NF.Lit
#' @author Dominic LaRoche
#'
NF.Lit <- function(x){
  s <- ifelse(x < 2.32, dnorm(x, 2.32, 0.18)/2.202707,
              ifelse(x > 3.58, dnorm(x, 3.58, 0.31)/1.284235, 1))
  return(s)
}
