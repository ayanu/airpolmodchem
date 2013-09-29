\name{vmr.2.nc}
\alias{vmr.2.nc}
\title{Convert volume mixing ratios to number concentrations.}
\description{
Convert volume mixing ratios in ppb to number concentrations 1/cm3.
}
\usage{
vmr.2.nc(vmr, TT = 20, pp = 1013.25)
}
\arguments{
  \item{vmr}{ volume mixing ratio in ppb }
  \item{TT}{ ambient temperature in °C, default 20 }
  \item{pp}{ ambient pressure in hPa, default 1013.25 }
}
\value{
    Number concentration in units 1/cm3. 
}
\author{ Stephan Henne (stephan.henne@emap.ch) }
\seealso{ \code{\link{mc.2.vmr}}, \code{\link{vmr.2.mc}} }
\keyword{ manip }

