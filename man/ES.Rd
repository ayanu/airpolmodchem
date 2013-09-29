\name{ES}
\alias{ES}
\title{Calculate saturation vapor pressure above water}
\description{
\code{ES} calculates the saturation vapor pressure above water from a given temperature.
}
\usage{
ES(tt)
}
\arguments{
  \item{tt}{temperature of water (units interpreted as K if larger 150, otherwise °C. Do not mix units in same vector.)}
}
\details{
    The formula 6.112 (hPa) *exp(17.62*tt / (243.12+tt)) is used if tt is in (K).
}
\value{
  the saturation vapor pressure in hPa
}
\author{Stephan Henne \email{stephan.henne@empa.ch}}

\seealso{ \code{\link{E.2.TD}}, \code{\link{ES.ice}}}
\examples{
#   Saturation water vapor pressure at freezing point
    print(ES(273.15))   # assuming (K)
    print(ES(0))        # assuming (°C)
}
\keyword{manip}% at least one, from doc/KEYWORDS
