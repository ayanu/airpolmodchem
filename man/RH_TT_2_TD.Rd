\name{RH_TT_2_TD}
\alias{RH_TT_2_TD}
\title{Calculate dew point temperature of an air parcel}
\description{
Calculate dew point temperature of an air parcel (°C) from the air parcel's relatve humidity (\%) and its ambient temperature (K or °C).
}
\usage{
RH_TT_2_TD(rh, tt)
}
\arguments{
  \item{rh}{relative humidity (\%)}
  \item{tt}{ambient temperature (K if >150, °C if <150)}
}
\value{
dew point temperature (°C)
}
\author{Stephan Henne}
\examples{
## 
print(RH_TT_2_TD(100, 20))
}
\keyword{manip}% at least one, from doc/KEYWORDS
