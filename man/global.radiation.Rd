\name{global.radiation}
\alias{global.radiation}
\title{Calculate potential global radiation}
\description{
\code{global.radiation} calculates the potential globl radiation at the earth's surface for a given location and time
}
\usage{
global.radiation(tm, lng = 0, lat = 0)
}
\arguments{
  \item{tm}{time/date object of class chron}
  \item{lng}{longitude in decimal degrees, negative for west}
  \item{lat}{latitude in decimal degrees, negative for south}
}
\details{
The function \code{global.radiation} calculates the potential global radiation at the earth's surface for a given loaction and time under clear sky conditions. 
The sun declination is calculated using formula given by Madronich 1999. Average transmisivity of the atmosphere as given by Stull 1988.
}
\value{
    global radiation (W/m\^2)
}
\references{Madronich, S., and S. Flocke (1999), The Role of Solar Radiation in Atmospheric Chemistry, in The Handbook of Environmental Chemistry Vol.2 Part L Environmental Photochemistry, edited by P. Boule, Springer, Berlin, Heidelberg, New York.
Stull, R. B. (1988), An Introduction to Boundary Layer Meteorology, 666 pp., Kluwer Academic Publishers, Dordrecht.
}
\author{Stephan Henne \email{stephan.henne@empa.ch}}
\examples{
## plot diurnal cycle of global radiation for 45°N and 10°E
    st = chron("01/01/01", "00:00:00")
    dtm = seq(st, st+1, by=1/24)
    plot(dtm, global.radiation(dtm), type="b", xlab="Time", ylab="Global Radiation (W/m\^2)")
##  plot global radiation worldwide at current time
    
    lat = matrix(rep(seq(-90, 90, 10),36), ncol=36)
    lng = matrix(rep(seq(-180, 170, 10),19), ncol=36, byrow=TRUE)
    gr = matrix(global.radiation(as.chron(Sys.time()), lng, lat), ncol=36)
    contour(x=seq(-180, 170, 10), y=seq(-90, 90, 10), t(gr), xlim=c(-180, 170), ylim=c(-90,90), xlab="Longitude (°E)", ylab="Latitude (°N)")
    library(maps)
    map(add=TRUE)
}
\keyword{manip}% at least one, from doc/KEYWORDS
