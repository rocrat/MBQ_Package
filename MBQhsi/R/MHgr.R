#' @title Mary Hunnicut Grasshopper Abundance HSI
#'
#' @description Calculates the partial HSI given the grasshopper abundance
#'
#' @param x A vector of grasshopper abundance during the breeding season on each acre in arbitrary units
#' @param loc The index of the abundance measure in the supplied vector to be assigned a suitability value
#'
#' @return Returns the relative HSI value
#'
#' @usage MH.gr(x, loc)
#'
#' @details Grasshopper abundance during the breeding season on each acre.  The exact relationship is unknown but more is better.  The given function will assign suitability to an area relative to other measured areas.
#'

MH.gr <- function(x, loc){
  s <- rank(x) / length(x)
  return(s[loc])
}
