\name{isothermal.pressure.gradient}
\alias{isothermal.pressure.gradient}
\title{
Pressure decrease for isothermal layer
}
\description{
Calculates the pressure at an altitude increment assuming an isothermal layer (constant temperature).
}
\usage{
isothermal.pressure.gradient(z, p0 = 1013.25, TT = 288.15)
}
\arguments{
  \item{z}{ Altitude increment in units m }
  \item{p0}{ Pressure at bottom of layer  }
  \item{TT}{ Mean temperature of layer in K or °C }
}
\details{
Calculates the pressure at an altitude increment z assuming an isothermal layer with temperature TT. 
The pressure at the bottom of the layer needs to be given by p0.
The result will have the same pressure units as used for the input pressure p0. 
}
\value{
Pressure at altitude z in units of p0.
}
\references{
Uses isothermal formulation of the barometric formula. 
}
\author{
stephan.henne@empa.ch
}

\seealso{
\code{\link{polytropic.pressure.gradient}}
}
\keyword{manip}
