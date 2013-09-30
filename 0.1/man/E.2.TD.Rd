\name{E.2.TD}
\alias{E.2.TD}
\title{Calculate dew point temperature for condensation for a given water vapor pressure}
\description{
Calculate dew point temperature for condensation for a given water vapor pressure.
}
\usage{
E.2.TD(e)
}
\arguments{
  \item{e}{water vapor pressure (hPa)}
}
\details{
Calculates the temperature (K) for which at a given water vapor pressure (hPa) condensation would set in.
}
\value{
    dew point temperature (K)
}
\author{Stephan Henne \email{stephan.henne@empa.ch}}
\seealso{\code{\link{ES}}}
\examples{
#   get water surface temperature at 20 hPa
    print(E.2.TD(20))
}
\keyword{manip}% at least one, from doc/KEYWORDS
