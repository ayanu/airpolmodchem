\name{E.2.TD.ice}
\alias{E.2.TD.ice}
\title{Calculate dew point temperature for deposition (freezing) for a given water vapor pressure}
\description{
Calculate dew point temperature for deposition (freezing) for a given water vapor pressure.
}
\usage{
E.2.TD.ice(e)
}
\arguments{
  \item{e}{water vapor pressure (hPa)}
}
\details{
Calculates the temperature (K) of an ice surface for which at a given water vapor pressure (hPa) freezing (deposition) would set in.
}
\value{
    deposition (freezing) point temperature (K)
}
\author{Stephan Henne \email{stephan.henne@empa.ch}}
\seealso{\code{\link{ES.ice}}}
\examples{
#   get water surface temperature at 20 hPa
    print(E.2.TD.ice(20))
}
\keyword{manip}% at least one, from doc/KEYWORDS
