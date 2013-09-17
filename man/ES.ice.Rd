\name{ES.ice}
\alias{ES.ice}
\title{Calculate saturation vapor pressure above ice}
\description{
\code{ES.ice} calculates the saturation vapor pressure above ice from a given temperature.
}
\usage{
ES.ice(tt)
}
\arguments{
  \item{tt}{temperature of ice (interpreted as K if larger 150, otherwise °C}
}
\details{
    the formula 6.112 (hPa) *exp(22.46*tt/(272.62+tt)) is used if tt is in (K)
}
\value{
  the saturation vapor pressure in hPa
}
\author{Stephan Henne \email{stephan.henne@empa.ch}}

\seealso{ \code{\link{ES}}}
\examples{
#   should yield the same results
    print(ES.ice(273.15))   # assuming (K)
    print(ES.ice(0))        # assuming (°C)
}
\keyword{manip}% at least one, from doc/KEYWORDS
