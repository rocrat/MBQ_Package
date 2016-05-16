#' @title Literature Forb Cover
#' @description Calculates the partial HSI given the forb cover.
#'
#' @param x Percent aerial forb cover measured as in a Daubenmire plot (Daubenmire 1959).
#' @param author The paper from which to calculate the partion HSI, c("king", "simms")
#'
#' @return Returns the relative HSI value
#'
#' @usage FC.Lit(x, author)
#'
#'
FC.Lit <- function(x, author){
  if(length(author) > 1) stop("Please choose only one author")
  if(!author %in% c("simms", "king")) stop("Please select either 'simms' or 'king'")

  if(author == "king"){
    s <- dnorm(x, 32.39, 2.55) * 6.391949
  }
  if(author == "simms"){
    s <- dnorm(x, 15, 4) * 10.02651
  }
  return(s)
}
