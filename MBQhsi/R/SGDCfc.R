#' @title Dr.s Sally Gall and Dan Cohan Forb Cover
#'
#' @description Calculates the partial HSI given the forb cover.
#'
#' @param x Forb cover measured as the average percent canopy cover dominated by forbs.  The optimal canopy cover of forbs differs between the fall/winter and spring/summer.
#' @param summer If TRUE then returns the partial HSI for summer, otherwise it returns the partial HSI for winter (default = TRUE).
#'
#' @return Returns the relative HSI value
#'
#' @usage FC.SGDC(x, summer = TRUE)
#'
#'
FC.SGDC <- function(x, summer = TRUE){
  if(summer){
    s <- ifelse(x <= 0.35, dbeta(x, 4, 4)/1.648437, ifelse( x >= 0.65, (dbeta(x, 4, 4))/1.648437, 1))
  }else{
    s <- ifelse(x <= 0.5, dbeta(x, 30, 30)/6.15469, ifelse(x >= 0.6, 0.2 + (dbeta(x, 30, 24.54)/6.3), 1))
  }
  return(s)
}
