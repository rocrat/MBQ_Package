#' @title Literature Visual Cover
#'
#' @description Calculates the partial HSI given the visual.
#'
#' @param x From Simms (1989): Visual cover measured as percent visual obstruction of a vertical range pole (Robel et al. 1970) at a height of 1m and distance of 4m.
#'
#' @return Returns the relative HSI value
#'
#' @usage VC.Lit(x)
#'
#'
VC.Lit <- function(x){
  s <- (pnorm(x, 83, 5))
  return(s)
}
