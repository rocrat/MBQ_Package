#' @title Literature Percent Bare Ground
#' @description Calculates the partial HSI given the percent bare ground.
#'
#' @param x From King (1998): Percent bare ground as a cover percentage measured by the Daubenmire method (Daubenmire 1959).
#' @param author The author of the paper from which the hsi function is estimated, one of c("simms", "guthery").
#'
#' @return Returns the relative HSI value
#'
#' @usage BG.Lit(x, author)
#'
#'
BG.Lit <- function(x, author){
  if(length(author) > 1) stop("Please choose only one author")
  if(!author %in% c("simms", "guthery")) stop("Please select either 'simms' or 'guthery'")
  if(author == "simms"){
    s <- dnorm(x, 11.3, 1.04) * 2.606894
  }
  if(author == "guthery"){
    s <- 1 - pnorm(x, 30, 5)
  }
  return(s)
}
