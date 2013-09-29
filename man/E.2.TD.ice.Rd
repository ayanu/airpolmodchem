\name{E.2.TD.ice}
\alias{E.2.TD.ice}
\title{Calculate temperature of ice surface given a certain water vapor pressure}
\description{
Calculates the temperature of an ice surface for a given water vapor pressure.
}
\usage{
E.2.TD.ice(e)
}
\arguments{
  \item{e}{water vapor pressure (hPa)}
}
\details{
Calculates the temperature (K) of an ice surface for which at a given water vapor pressure (hPa) deposition would set in.
}
\value{
    temperature of water surface (K)
}
\author{Stephan Henne \email{stephan.henne@empa.ch}}
\seealso{\code{\link{ES.ice}}}
\examples{
#   get water surface temperature at 20 hPa
    print(E.2.TD.ice(20))
}
\keyword{manip}% at least one, from doc/KEYWORDS
