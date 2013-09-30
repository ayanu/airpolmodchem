\name{RH.TT.PP.2.SH}
\alias{RH.TT.PP.2.SH}
\title{Calculate specific humidity from relative humidity, ambient temperature, and pressure}
\description{
\code{RH.TT.PP.2.SH} calculates the specific humidity (kg/kg) of an air parcel from its relative humidity (\%), temperature (K or °C) and pressure (hPa).
}
\usage{
RH.TT.PP.2.SH(rh, tt, pp)
}
\arguments{
  \item{rh}{relative humidity of air parcel (\%)}
  \item{tt}{temperature of air parcel (K (>150) or °C (<150)}
  \item{pp}{ambient pressure (hPa)}
}
\value{
specific humidity of air parcel in kg water per kg air (kg/kg)
}
\author{Stephan Henne \email{stephan.henne@empa.ch}}
\examples{
## 
print(RH.TT.PP.2.SH(100, 10, 1000))
print(RH.TT.PP.2.SH(100, 25, 1000))
}
\keyword{manip}% at least one, from doc/KEYWORDS
