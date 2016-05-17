#' @title Dr. Roy Tomlinson HSI Function
#'
#' @description Calculates the HSI given the values of forb cover, forb height, grass cover, grass height, shrub cover, shrub height, tree cover, bare ground, forb diversity, and grass diversity.
#'
#' @param fcs   Forb Cover in spring/summer measured as the average total canopy cover of forbs on a on a typical home range (10.9 ha).
#' @param fcw   Forb cover in fall/winter measured as the average total canopy cover of forbs on a on a typical home range (10.9 ha).
#' @param fd   Forb Diversity measured as the total number of forb species  on a typical  home range (10.9 ha).
#' @param fh   Forb Height measured as the average height of forbs on a typical  home range (10.9 ha).
#' @param gc   Grass Canopy Cover measured from above the grass canopy as the amount of ground covered by grass foliage  on a typical  home range (10.9 ha).
#' @param gd   Grass Diversity measured as the total number of both annual and perennial grass species  on a typical  home range (10.9 ha).
#' @param gh   Grass Height measured as the average height of grass  on a typical  home range (10.9 ha).
#' @param tc   Tree and shrub cover measured as the percent canopy cover of trees on a typical  home range (10.9 ha).
#' @param bg   Bare Ground measured as the proportion of surface area not occupied by stems or other obstructions on a typical  home range (10.9 ha).
#'
#' @return Returns the relative HSI value
#'
#' @usage HSI.RT(fcs, fcw, fd, fh, gc, gd, gh, tc, bg)
#'
#'

HSI.RT <- function(fcs, fcw, fd, fh, gc, gd, gh, tc, bg){
  f <- Food.RT(gd, gc, fd, fcs, fcw)
  c <- Cover.RT(fh, gh, fcs, fcw, gc, tc, bg)
  s <- rep(0, length(f))
  for(i in 1:length(f)){
    s[i] <- min(f[i], c[i])
  }
  return(s)
}

FC.min <- function(fcs, fcw){
  s <- min(FC.RT(fcs, summer = TRUE), FC.RT(fcw, summer = FALSE))
}

TC.min <- function(tc){
  s <- min(TC.RT(tc, summer = TRUE), TC.RT(tc, summer = FALSE))
}

Food.RT<-function(gd, gc, fd, fcs, fcw){
  s<-(((GD.RT(gd) * GC.RT(gc))^.5) + ((FD.RT(fd) * FC.min(fcs, fcw))^.5))/2
  return(s)
}

Cover.RT <- function(fh, gh, fcs, fcw, gc, tc, bg){
  s <- (FH.RT(fh) + GH.RT(gh) + FC.RT(fcs, fcw) + GC.RT(gc) + TC.min(tc) + BG.RT(bg))/6
  return(s)
}
