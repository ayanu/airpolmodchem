\name{RH_TT_PP_2_SH}
\alias{RH_TT_PP_2_SH}
\title{Calculate specific humidity from relative humidity, ambient temperature, and pressure}
\description{
\code{RH_TT_PP_2_SH} calculates the specific humidity (g/kg) of an air parcel from its relative humidity (\%), temperature (K or °C) and pressure (hPa).
}
\usage{
RH_TT_PP_2_SH(rh, tt, pp)
}
\arguments{
  \item{rh}{relative humidity of air parcel (\%)}
  \item{tt}{temperature of air parcel (K (>150) or °C (<150)}
  \item{pp}{ambient pressure (hPa)}
}
\value{
specific humidity of air parcel in g water per kg air (g/kg)
}
\author{Stephan Henne \email{stephan.henne@empa.ch}}
\examples{
## 
print(RH_TT_PP_2_SH(100, 10, 1000))
print(RH_TT_PP_2_SH(100, 25, 1000))
}
\keyword{manip}% at least one, from doc/KEYWORDS
