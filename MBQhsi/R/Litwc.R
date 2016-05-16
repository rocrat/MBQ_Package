#' @title Literature Woody Cover
#'
#' @description Calculates the partial HSI given the woody cover.
#'
#' @param x Percent woody cover as measured in a line intercept method (Canfield 1941).
#' @param AZ If TRUE, the partial HSI for Arizona is returned. Otherwise the partial HSI for Mexico is returned. (ignored ofr author == 'king')
#' @param author The author of the paper of the model to be esitmated. Choose one of either "king" or "guthery".
#'
#' @return Returns the relative HSI value
#'
#' @usage WC.Lit(x, AZ = TRUE, author)
#'
#'
WC.Lit <- function(x, AZ = TRUE, author){
  if(length(author) > 1) stop("Please choose only one author")
  if(!author %in% c("king", "guthery")) stop("Please select either 'king' or 'guthery'")
  if(author == "guthery"){
    if(AZ){
      s <- dnorm(x, 15, 15) * 37.59943
    }else{
      s <- ifelse(x < 15, y <- dnorm(x, 27.5, 12.5) * 51.65914,
                  ifelse(x > 40, y <- dnorm(x, 27.5, 12.5) * 51.65914, y <- 1))
    }
  }
  if(author == "king"){
    s <- dnorm(x, 12.38, 1.6) * 4.010919
  }
  return(s)
}
