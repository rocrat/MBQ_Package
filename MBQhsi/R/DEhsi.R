#' @title Dave Ellis HSI Function
#'
#' @description Calculates the HSI given the values of forb cover, grass cover, shrub cover, tree cover, forb diversity, and grass diversity.
#'
#' @param fh   Forb Height measured as the average height of Forbs on a given home range in centimeters.
#' @param gh   Grass Height measured as the average height of grass on a given home range (in meters).
#' @param sh   Shrub Height measured as the average height of grass on a given home range (in meters).
#' @param tc   Tree cover measured as the percent canopy cover of trees on a given home range.
#' @param gcc   Grass Canopy Cover measured from above the grass canopy as the amount of ground covered by grass foliage on a given home range.
#' @param gcs  Grass Cover from the side measured as the average amount of distance (in meters) until complete visual obstruction on a given home range.
#' @param gcb  Grass basal area measured as the average area occupied by stems of grass on a given home range.
#' @param fd   Forb Diversity measured as the total number of forb species on a given home range throughout the year.
#' @param gd   Diversity measured as the total number of both annual and perennial grass species on a given home range throughout the year.
#' @param sd   Shrub diversity measured as the total number of shrub species on  a given home range throughout the year.
#' @param totc   Total Cover measured as the average total canopy cover of all vegetation (and brush piles) on a given home range.  Suitability of total cover differs in winter and summer.
#' @param Summer   TRUE if measured in summer (default = TRUE).
#'
#' @return Returns the relative HSI value
#'
#' @usage HSI.Ellis(fh, gh, sh, tc, gcc, gcs, gcb, fd, gd, sd, totc, Summer = TRUE)
#'
#'
HSI.Ellis<-function(fh, gh, sh, tc, gcc, gcs, gcb, fd, gd, sd, totc, Summer = TRUE){
  p <- Pred.Ellis(fh, gh, sh, tc, fd, gd, sd, gcc, gcs, gcb, totc, Summer)
  f <- SDP.Ellis(fd, gd, sd)
  s <- vector()
  for(i in 1:length(p)){
    s[i] <- min(p[i], f[i])
  }
  return(s)
}

SDP.Ellis <- function(fd, gd, sd){
  s <- (FD.Ellis(fd) + GD.Ellis(gd) + SD.Ellis(sd))/3
  return(s)
}

Pred.Ellis<-function(fh, gh, sh, tc, fd, gd, sd, gcc, gcs, gcb, totc, Summer){
  GC.Ellis <- (GCC.Ellis(gcc) + GCS.Ellis(gcs) + GCB.Ellis(gcb))/3
  p <- (((FH.Ellis(fh) + GH.Ellis(gh) + SH.Ellis(sh) + GC.Ellis + TotC.Ellis(totc, Summer) + TC.Ellis(tc))/6) * SDP.Ellis(fd, gd, sd))^.5
  return(p)
}
