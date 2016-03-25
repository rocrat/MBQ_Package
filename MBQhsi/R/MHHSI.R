#' @title Mary Hunnicut HSI Function
#'
#' @description Calculates the HSI given the values of forb cover, grass cover, shrub cover, tree cover, bare ground, nest sites, grasshopper abundance, forb diversity, shrub diversity, grass diversity, layered vegetation, and ecotone presence.
#'
#' @param fc Proportion (in decimal form) of green forbs measured as one would in a Daubnenmire frame (per acre).
#' @param gc Proportion (in decimal form) of green grass measured as one would in a Daubnenmire frame (per acre).
#' @param sc The proportion of shrub cover (in decimal form).
#' @param tc Canopy Cover (\%) of trees as measured by a densitometer or aerial photo over each acre of ground.
#' @param bg The proportion of bare ground (in decimal form).
#' @param nc The number of appropriate nest sites, as counted in a frame or intercept technique, per acre.
#' @param gr.x A vector of grasshopper abundance during the breeding season on each acre in arbitrary units
#' @param gr.loc The index of the grasshopper abundance measure in the supplied vector of grasshopper abundances to be assigned a suitability value.
#' @param fd The number of forb species per acre, as measured on a line intercept or frame method.
#' @param sd The number of shrub and tree species per acre as counted on a line transect or quadrant.
#' @param gd The number of native grass species per acre as counted on a line intercept.
#' @param lv A 1 or 0 indicating the presence or absence of multi-layered vegetation on each acre.
#' @param eh A 1 or 0 indicating the presence or absence of an ecotone.
#'
#' @return Returns the relative HSI value
#'
#' @usage MH.HSI(fc, gc, sc, tc, bg, nc, gr.x, gr.loc, fd, sd, gd, lv, eh)
#'
#'
MH.HSI <- function(fc, gc, sc, tc, bg, nc, gr.x, gr.loc, fd, sd, gd, lv, eh){
  f <- Food.Mary(sc, bg, fc, gc, gr.x, gr.loc, gd, fd, sd)
  c <- Cover.Mary(tc, sc, fc, gc, eh, lv, sd)
  r <- Reproduction.Mary(tc, bg, sc, nc, fc, gc, gr.x, gr.loc, fd, sd)
  s <- rep(0, length(r))
  for(i in 1:length(f)){
    s[i] <- min(f[i], c[i], r[i])
  }
  return(s)
}

Food.Mary <- function(sc, bg, fc, gc, gr.x, gr.loc, gd, fd, sd){#need to figure out what GU is???
  f <- (MH.sc(sc) + MH.bg(bg) + MH.fcgc(fc) + MH.fcgc(gc) + MH.gr(x = gr.x, loc = gr.loc) + MH.gd(gd) + MH.fd(fd) + MH.std(sd))/8
  return(f)
}

Cover.Mary <- function(tc, sc, fc, gc, eh, lv, sd){
  c <- (MH.tc(tc) + MH.sc(sc) + MH.fcgc(fc) + MH.fcgc(gc) + MH.eh(eh) + MH.lv(lv) + MH.std(sd))/7
  return(c)
}

Reproduction.Mary <- function(tc, bg, sc, nc, fc, gc, gr.x, gr.loc, fd, sd){#GU is here too
  r <- (MH.tc(tc) + MH.bg(bg) + MH.sc(sc) + MH.nc(nc) + MH.fcgc(fc) + MH.fcgc(gc) + MH.gr(x = gr.x, loc = gr.loc) + MH.fd(fd) + MH.std(sd))/9
  return(r)
}
