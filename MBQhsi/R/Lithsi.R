#' @title Literature HSI Function
#'
#' @description Calculates the HSI given the values of woody stem density, woody cover, tree and shrub cover, number of trees, visual cover, disc of vulnerability, cone of vulnerability, number of forbs, forb diversity, forb cover, grass cover, basal grass cover, Lehmann's lovegrass cover, bare ground, half-shrub cover, and operative temperature.
#'
#' @param wsd   From King (1998): The mean number of woody stems >1m tall per 200 square meters.
#' @param wc   Percent woody cover as measured in a line intercept method (Canfield 1941).
#' @param tc   From Simms (1989): Percent tree and shrub cover as measured by a line intercept method (Canfield 1941).
#' @param nt   From Simms (1989): Number of trees with a height between 0 and 5 meters per hectare.
#' @param vc   From Simms (1989): Visual cover measured as percent visual obstruction of a vertical range pole (Robel et al. 1970) at a height of 1m and distance of 4m.
#' @param dv   From Guthery et al. (2000): Visual cover measured as a disc of vulnerability (as described in Kopp (1998)) around a random point.
#' @param cv   From Guthery et al.  (2000):   Visual cover measured as a cone of vulnerability (as described in Kopp (1998)) measured in millions of cubic meters around a random point.
#' @param nf   From King (1998): The number of forbs counted per 1000 square centimeters (as in a Daubenmire frame (Daubenmire 1959)).
#' @param fd   From Simms (1989): Forb diversity as measured by the mean number of forb species as measured by the Daubenmire method (Daubenmire 1959).
#' @param fc   Percent aerial forb cover measured as in a Daubenmire plot (Daubenmire 1959).
#' @param gc   From Simms (1989): Percent aerial grass cover measured as in a Daubenmire plot (Daubenmire 1959).
#' @param gcb  From Simms (1989): Percent basal grass cover measured as in a Daubenmire plot (Daubenmire 1959).
#' @param llg  From Simms (1989): Percent cover of Lehmann’s lovegrass measured by the Daubenmire method (Daubenmire 1959).
#' @param hsc  From Simms (1989):  Percent half-shrub cover measured by the Daubenmire method (Daubenmire 1959).
#' @param bg   From King (1998): Percent bare ground as a cover percentage measured by the Daubenmire method (Daubenmire 1959).
#' @param ot   From Guthery et al. (2001): Operative temperature measured as described in Forrester et al. (1998).
#' @param AZ   If TRUE, the partial HSI for Arizona is returned.  Otherwise, the partial HSI for Mexico is returned.
#'
#' @return Returns the relative HSI value
#'
#' @usage HSI.Lit(wsd, wc, tc, nt, vc, dv, cv, nf, fd, fc, gc, gcb, llg, hsc, bg, ot, AZ = TRUE)
#'
#'

HSI.Lit <- function(wsd, wc, tc, nt, vc, dv, cv, nf, fd, fc, gc, gcb, llg, hsc, bg, ot, AZ = TRUE){
  r <- Repro.Lit(wsd, wc, tc, nt, nf, fd, fc)
  f <- Food.Lit(nf, fd, fc, gc, gcb, llg, hsc)
  c <- Cover.Lit(wsd, wc, tc, nt, vc, dv, cv, AZ, nf, fd, fc, gc, gcb, llg, hsc, bg)
  s <- rep(0, length(f))
  for(i in 1:length(f)){
    s[i] <- min(r[i], f[i], c[i], ot[i])
  }
  return(s)
}

WoodyCover.Lit <- function(wsd, wc, tc, nt){
  s <- (WSD.Lit(wsd) +
          (WC.Lit(wc, author = 'guthery') + WC.Lit(wc, author = 'king'))/2 +
          TC.Lit(tc) +
          NT.Lit(nt))/4
  return(s)
}

VisualCover.Lit <- function(vc, dv, cv, AZ){
  s <- (VC.Lit(vc) + DV.Lit(dv) + CV.Lit(cv, AZ = AZ))/3
  return(s)
}

Forbs.Lit <- function(nf, fd, fc){
  s <- (NF.Lit(nf) +
          FD.Lit(fd) +
          FC.Lit(fc, author = 'simms') +
          FC.Lit(fc, author = 'king'))/4
  return(s)
}

Grasses.Lit <- function(gc, gcb, llg){
  s <- (GC.Lit(gc) + GCB.Lit(gcb) + LLG.Lit(llg))/3
  return(s)
}

Cover.Lit <- function(wsd, wc, tc, nt, vc, dv, cv, AZ, nf, fd, fc, gc, gcb, llg, hsc, bg){
  s <- (WoodyCover.Lit(wsd, wc, tc, nt) +
          VisualCover.Lit(vc, dv, cv, AZ = AZ) +
          Forbs.Lit(nf, fd, fc) +
          Grasses.Lit(gc, gcb, llg) +
          HSC.Lit(hsc) +
          (BG.Lit(bg, author = 'simms') + BG.Lit(bg, author = 'guthery'))/2)/6
  return(s)
}

Food.Lit <- function(nf, fd, fc, gc, gcb, llg, hsc){
  s <- (Forbs.Lit(nf, fd, fc) +
          Grasses.Lit(gc, gcb, llg) +
          HSC.Lit(hsc))/3
  return(s)
}

Repro.Lit <- function(wsd, wc, tc, nt, nf, fd, fc){
  s <- (WoodyCover.Lit(wsd, wc, tc, nt) + Forbs.Lit(nf, fd, fc))/2
}
