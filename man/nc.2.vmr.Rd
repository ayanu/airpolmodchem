\name{nc.2.vmr}
\alias{nc.2.vmr}
\title{Convert number concentrations to volume mixing ratios.}
\description{
Convert number concentrations 1/cm3 to volume mixing ratios in ppb .
}
\usage{
nc.2.vmr(nc, TT = 20, pp = 1013.25)
}
\arguments{
  \item{nc}{ Number concentration in units 1/cm3 }
  \item{TT}{ ambient temperature in K, if < 150 assumed to be in °C, default 20 }
  \item{pp}{ ambient pressure in hPa, default 1013.25 }
}
\value{   
    volume mixing ratio in ppb 
}
\author{ Stephan Henne (stephan.henne@emap.ch) }
\seealso{ \code{\link{mc.2.vmr}}, \code{\link{vmr.2.mc}} }
\keyword{ manip }

