\name{ES.inv}
\alias{ES.inv}
\title{Calculate temperature of water surface for given saturation water vapor pressure}
\description{
Calculates the temperature of a water surface for a given saturation water vapor pressure.
}
\usage{
ES.inv(es)
}
\arguments{
  \item{es}{saturation water vapor pressure (hPa)}
}
\details{
Calculates the temperature (K) of a water surface for a given saturation water vapor pressure (hPa).
}
\value{
    temperature of water surface (K)
}
\author{Stephan Henne \email{stephan.henne@empa.ch}}
\seealso{\code{\link{ES}}}
\examples{
#   get water surface temperature at 20 hPa
    print(ES.inv(20))
    print(ES(ES.inv(20)))
}
\keyword{manip}% at least one, from doc/KEYWORDS
