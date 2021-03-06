% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JGsc.R
\name{SC.Goodwin}
\alias{SC.Goodwin}
\title{John Goodwin Shrub Cover}
\usage{
SC.Goodwin(x, AZ = TRUE)
}
\arguments{
\item{x}{Shrub cover measured as the average canopy cover of shrubs throughout the year. Optimal levels of grass diversity differ in Arizona and Mexico.}

\item{AZ}{True or False.  Is the habitat in Arizona (TRUE) or Mexico (FALSE).  Defaults to Arizona.}
}
\value{
Returns the relative HSI value
}
\description{
Calculates the partial HSI given the shrub cover.
}
\details{
Shrub cover should be distributed in clumps approximately 100 yards apart.
}
\author{
Dominic LaRoche
}

