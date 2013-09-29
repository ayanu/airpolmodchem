\name{E.2.TD}
\alias{E.2.TD}
\title{Calculate temperature of water surface for given a certain water vapor pressure}
\description{
Calculates the temperature of a water surface for a given water vapor pressure.
}
\usage{
E.2.TD(e)
}
\arguments{
  \item{e}{water vapor pressure (hPa)}
}
\details{
Calculates the temperature (K) of an water surface for which at a given water vapor pressure (hPa) deposition would set in.
}
\value{
    temperature of water surface (K)
}
\author{Stephan Henne \email{stephan.henne@empa.ch}}
\seealso{\code{\link{ES}}}
\examples{
#   get water surface temperature at 20 hPa
    print(E.2.TD(20))
}
\keyword{manip}% at least one, from doc/KEYWORDS
