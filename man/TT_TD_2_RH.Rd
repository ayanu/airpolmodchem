\name{TT.TD.2.RH}
\alias{TT.TD.2.RH}
\title{calculate relative humidity from ambient and dew point temperature}
\description{
\code{TT.TD.2.RH} calculates the relative humidity (\%) of an air parcel from its ambient temperature (K or °C) and dew point temperature (K or °C)
}
\usage{
TT.TD.2.RH(tt, td)
}
\arguments{
  \item{tt}{ambient temperature (K >150 or °C<150)}
  \item{td}{dew point temperature (K >150 or °C<150)}
}
\details{
\code{TT.TD.2.RH} calculates the relative humidity (\%) of an air parcel from its ambient temperature (K or °C) and dew point temperature (K or °C).
}
\value{
    relative humidity (\%)
}
\author{Stehpan Henne}
\examples{
    print(TT.TD.2.RH(20, 20))
    print(TT.TD.2.RH(20, 10))
}
\keyword{manip}% at least one, from doc/KEYWORDS
