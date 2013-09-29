\name{RH.TT.2.E}
\alias{RH.TT.2.E}
\title{Calculate water vapor partial pressure of an air parcel}
\description{
Calculate water vapor partial pressure an air parcel (hPa) from the air parcel's relatve humidity (\%) and its ambient temperature (K or °C).
}
\usage{
RH.TT.2.E(rh, tt)
}
\arguments{
  \item{rh}{relative humidity (\%)}
  \item{tt}{ambient temperature (K if >150, °C if <150)}
}
\value{
water vapor partial pressure (hPa)
}
\author{Stephan Henne}
\examples{
## 	Water vapor partial pressure at 20°C and 100 relative humidity
print(RH.TT.2.E(100, 20))
}
\keyword{manip}% at least one, from doc/KEYWORDS
