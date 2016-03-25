#' @title Mary Hunnicut Nest Site HSI
#'
#' @description Calculates the partial HSI given a the number of appropriate nest sites
#'
#' @param x The number of appropriate nest sites, as counted in a frame or intercept technique, per acre.
#'
#' @return Returns the relative HSI value
#'
#' @usage MH.nc(x)
#'
#' @details Nests are typically located in a 9 inch (23cm) round clump of grass but can be located at the base of a shrub or under other similarly dense vegetation.  In flight pens, masked bobwhites will even use artificial structures for nesting cover.  Appropriate nest sites should be available at the rate of approximately 300-600 per acre.
#'
MH.nc <- function(x){
  s <- ifelse(x < 300, dnorm(x, 300, 100) * 250.6628, ifelse(x > 600, dnorm(x, 600, 200) * 501.327, 1))
  return(s)
}
