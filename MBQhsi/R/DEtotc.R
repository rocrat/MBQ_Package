#' @title Dr. Dave Ellis Total Cover
#'
#' @description Calculates the partial HSI given the total cover.
#'
#' @param x Total Cover measured as the average total canopy cover of all vegetation (and brush piles) on a given home range.  Suitability of total cover differs in winter and summer.
#' @param Summer TRUE if measured in summer (default = TRUE).
#'
#' @return Returns the relative HSI value
#'
#' @usage TotC.Ellis(x, Summer = TRUE)
#'
#'
TotC.Ellis <- function(x, Summer = TRUE){
  if(Summer){
    s <- dbeta(x, 4, 4)/2.2
  }else{
    s <- dbeta(x, 3, 7)/2.9
  }
  return(s)
}
