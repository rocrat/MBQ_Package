#' @title Dr.s Sally Gall and Dan Cohan Shrub Cover
#'
#' @description Calculates the partial HSI given the shrub cover.
#'
#' @param x Shrub cover measured as the average canopy cover of shrubs.  The two experts differed in their assessment of optimal shrub cover.
#' @param expert 1 or 2. If 1 the "expert 1" model is used (see LaRoche and Conway 2013), if 2 the "expert 2" model is used
#'
#' @return Returns the relative HSI value
#'
#' @usage SC.SGDC(x, expert = 1)
#'
#'
SC.SGDC <- function(x, expert = 1){
  if(expert == 1){
    s <- ifelse(x <= 0.3, dbeta(x, 3, 3)/1.875, ifelse(x >= 0.6, (dbeta(x, 3, 3))/1.728, 1))
  }else{
    s <- dbeta(x, 5, 7.5)/2.84164
  }
  return(s)
}
