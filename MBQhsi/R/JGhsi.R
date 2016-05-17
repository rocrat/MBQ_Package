#' @title John Goodwin HSI Function
#'
#' @description Calculates the HSI given the values of forb cover, grass cover, shrub cover, tree cover, forb diversity, and grass diversity.
#'
#' @param fc   Forb cover measured as the average canopy cover of forbs.
#' @param gc   Grass cover measured as the average canopy cover of grass.
#' @param sc   Shrub cover measured as the average canopy cover of shrubs throughout the year.
#' @param tc   Tree cover measured as the average canopy cover of trees.
#' @param fd   The number of forb species per acre, as measured on a line intercept or frame method.
#' @param gd   Grass Diversity measured as the total number of both annual and perennial grass species found in reasonable abundance on a given home range throughout the year.
#' @param AZ   True or False.  Is the habitat in Arizona (TRUE) or Mexico (FALSE).  Defaults to Arizona.
#'
#' @return Returns the relative HSI value
#'
#' @usage HSI.Goodwin(fd, fc, gd, gc, tc, sc, AZ = TRUE)
#'
#' @export
#' @name HSI.Goodwin
#' @author Dominic LaRoche
#'
HSI.Goodwin <- function(fd, fc, gd, gc, tc, sc, AZ = TRUE){
  f <- Food.goodwin(gd, fd, AZ)
  c <- Cover.goodwin(fd, fc, gd, gc, tc, sc, AZ)
  t <- Thermal.goodwin(fd, fc, tc, AZ)
  s <- vector()
  for(i in 1:length(f)){
    s[i] <- min(f[i], c[i], t[i])
  }
  return(s)
}

Food.goodwin <- function(gd, fd, AZ){
  f <- (GD.Goodwin(gd, AZ) + FD.Goodwin(fd))/2
  return(f)
}

Cover.goodwin<-function(fd, fc, gd, gc, tc, sc, AZ){
  c<-(((FD.Goodwin(fd) * FC.Goodwin(fc, AZ))^.5) +
        ((GD.Goodwin(gd, AZ) * GC.Goodwin(gc, AZ))^.5) +
        TC.Goodwin(tc, AZ) + SC.Goodwin(sc, AZ))/4
  return(c)
}

Thermal.goodwin<-function(fd, fc, tc, AZ){
  t<-((FD.Goodwin(fd) * FC.Goodwin(fc, AZ)^.5) + TC.Goodwin(tc, AZ))/2
  return(t)
}



