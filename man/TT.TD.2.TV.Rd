\name{TT.TD.PP.2.TV}
\alias{TT.TD.PP.2.TV}
\title{calculate virtual temperature from ambient and dew point temperature}
\description{
\code{TT.TD.PP.2.RH} calculates the virtual temperature of an air parcel from its ambient temperature (K or °C) and dew point temperature (K or °C)
}
\usage{
TT.TD.PP.2.TV(tt, td, pp=1013.25)
}
\arguments{
  \item{tt}{ambient temperature (K >150 or °C<150)}
  \item{td}{dew point temperature (K >150 or °C<150)}
  \item{pp}{ambient pressure (hPa)}  
}
\details{
\code{TT.TD.PP.2.TV} calculates the virtual temperature of an air parcel from its ambient temperature (K or °C) and dew point temperature (K or °C).
}
\value{
    virtual temperature in K
}
\author{Stehpan Henne}
\examples{
    
}
\keyword{manip}% at least one, from doc/KEYWORDS
