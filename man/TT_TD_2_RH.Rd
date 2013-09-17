\name{TT_TD_2_RH}
\alias{TT_TD_2_RH}
\title{calculate relative humidity from ambient and dew point temperature}
\description{
\code{TT_TD_2_RH} calculates the relative humidity (\%) of an air parcel from its ambient temperatuer (K or °C) and dew point temperature (K or °C)
}
\usage{
TT_TD_2_RH(tt, td)
}
\arguments{
  \item{tt}{ambient temperature (K >150 or °C<150)}
  \item{td}{dew point temperature (K >150 or °C<150)}
}
\details{
\code{TT_TD_2_RH} calculates the relative humidity (\%) of an air parcel from its ambient temperatuer (K or °C) and dew point temperature (K or °C).
}
\value{
    relative humidity (\%)
}
\author{Stehpan Henne}
\examples{
    print(TT_TD_2_RH(20, 20))
    print(TT_TD_2_RH(20, 10))
}
\keyword{manip}% at least one, from doc/KEYWORDS
