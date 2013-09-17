\name{vmr2nc}
\alias{vmr2nc}
\title{Convert volume mixing ratios to number concentrations.}
\description{
Convert volume mixing ratios in ppb to number concentrations 1/cm3.
}
\usage{
vmr2nc(vf, TT = 20, pp = 1013.25)
}
\arguments{
  \item{vf}{ volume mixing ratio in ppb }
  \item{TT}{ ambient temperature in °C, default 20 }
  \item{pp}{ ambient pressure in hPa, default 1000 }
}
\value{
    Number concentration 1/cm3 
}
\author{ Stephan Henne (stephan.henne@emap.ch) }
\seealso{ \code{\link{mc2vmr}}, \code{\link{vmr2mc}} }
\keyword{ manip }

