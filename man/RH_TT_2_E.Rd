\name{RH_TT_2_E}
\alias{RH_TT_2_E}
\title{Calculate water vapor partial pressure of an air parcel}
\description{
Calculate water vapor partial pressure an air parcel (hPa) from the air parcel's relatve humidity (\%) and its ambient temperature (K or °C).
}
\usage{
RH_TT_2_E(rh, tt)
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
## 
print(RH_TT_2_E(100, 20))
}
\keyword{manip}% at least one, from doc/KEYWORDS
