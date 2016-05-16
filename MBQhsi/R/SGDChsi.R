#' @title Dr.s Sally Gall and Dan Cohan HSI Function
#'
#' @description Calculates the HSI given the values of forb cover, forb height, grass cover, grass height, shrub cover, shrub height, tree cover, bare ground, forb diversity, and grass diversity.
#'
#' @param fcs   Forb cover in spring/summer measured as the average percent canopy cover dominated by forbs.
#' @param fcw   Forb cover in fall/winter measured as the average percent canopy cover dominated by forbs.
#' @param fd   Forb Diversity measured as the total number of forb species on a typical  home range (10.9 ha) throughout the year.
#' @param fhs   Forb height measured as the average height of forbs in spring/summer.
#' @param fhw   Forb height measured as the average height of forbs in fall/winter.
#' @param gcp   Perennial grass cover measured as the percent canopy cover of grass.
#' @param gca   Annual grass cover measured as the percent canopy cover of grass.
#' @param gdp   Perennial grass diversity measured as the total number of grass species found on a typical home range (10.9 ha).
#' @param gda   Annual grass diversity measured as the total number of grass species found on a typical home range (10.9 ha).
#' @param gh   Grass height measured as the average height of grass on a typical home range (10.9 ha).  The two experts differed on their assessment of optimal grass height.
#' @param sc   Shrub cover measured as the average canopy cover of shrubs.  The two experts differed in their assessment of optimal shrub cover.
#' @param sh   Shrub height measured as the average height of shrubs.
#' @param tc   Upland tree cover measured as the average canopy cover of trees.
#' @param tca   Arroyo tree cover measured as the average canopy cover of trees.
#' @param bg   Bare ground measured as the average canopy cover of bare ground.  Bare ground should be in the form of a matrix interspersed with other canopy components.
#' @param expert   1 or 2. If 1 the "expert 1" model is used (see LaRoche and Conway 2013), if 2 the "expert 2" model is used.
#'
#' @return Returns the relative HSI value
#'
#' @usage HSI.SGDC(fcs, fcw, fd, fhs, fhw, gcp, gca, gdp, gda, gh, sc, sh, tc, tca, bg, expert = 1)
#'
#'
HSI.SGDC <- function(fcs, fcw, fd, fhs, fhw, gcp, gca, gdp, gda, gh, sc, sh, tc, tca, bg, expert = 1){
  f <- Food.SGDC(fcs, fcw, fhs, fhw, fd, gh, gcp, gca, gdp, gda, sc, sh, expert = expert)
  r <- Repro.SGDC(tc, tca, gh, gcp, gca, gdp, gda, bg, expert = expert)
  c <- Cover.SGDC(tc, tca, gh, gcp, gca, gdp, gda, fcs, fcw, fhs, fhw, fd, sc, sh, bg, expert = expert)
  t <- TreeCover.SGDC(tc, tca)
  s <- rep(0, length(f))
  for(i in 1:length(f)){
    s[i] <- min(f[i], r[i], c[i], t[i])
  }
  return(s)
}

Forbs.SGDC <- function(fcs, fcw, fhs, fhw, fd){
  f <- (FC.SGDC(fcw, summer = FALSE) * FC.SGDC(fcs, summer = TRUE) * FH.SGDC(fhw, summer = FALSE) * FH.SGDC(fhs, summer = TRUE) * FD.SGDC(fd))^0.2
}

Grass.SGDC <- function(gh, gcp, gca, gdp, gda, expert){
  g <- (GH.SGDC(gh, expert = expert) * GC.SGDC(gcp, perennial = TRUE) * GC.SGDC(gca, perennial = FALSE) * GD.SGDC(gdp, perennial = TRUE) * GD.SGDC(gda, perennial = FALSE))^.2
}

Shrubs.SGDC <- function(sc, sh, expert){
  s <- (SC.SGDC(sc, expert = expert) * SH.SGDC(sh))^0.5
}

TreeCover.SGDC <- function(tc, tca){
  t <- (TC.SGDC(tc, uplands = TRUE) * TC.SGDC(tca, uplands = FALSE))^.5
}

Repro.SGDC <- function(tc, tca, gh, gcp, gca, gdp, gda, bg, expert){
  r <- (TreeCover.SGDC(tc, tca) +
          Grass.SGDC(gh, gcp, gca, gdp, gda, expert = expert) +
          BG.SGDC(bg))/3
  return(r)
}

Food.SGDC <- function(fcs, fcw, fhs, fhw, fd, gh, gcp, gca, gdp, gda, sc, sh, expert){
  f <- (Forbs.SGDC(fcs, fcw, fhs, fhw, fd) +
          Grass.SGDC(gh, gcp, gca, gdp, gda, expert = expert) +
          Shrubs.SGDC(sc, sh, expert = expert))/3
  return(f)
}

Cover.SGDC <- function( tc, tca, gh, gcp, gca, gdp, gda, fcs, fcw, fhs, fhw, fd, sc, sh, bg, expert){
  c <- (TreeCover.SGDC(tc, tca) +
          Grass.SGDC(gh, gcp, gca, gdp, gda, expert = expert) +
          Forbs.SGDC(fcs, fcw, fhs, fhw, fd) +
          Shrubs.SGDC(sc, sh, expert = expert) +
          BG.SGDC(bg))/5
  return(c)
}
