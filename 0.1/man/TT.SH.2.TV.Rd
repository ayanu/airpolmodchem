% 
% TODO: Write Documentation 
% 

\name{TT.SH.2.TV}
\alias{TT.SH.2.TV}
\title{calculate virtual temperature from temperature and specific humidity}
\description{
\code{TT.SH.2.TV} calculates the virtual temperature (K) of an air parcel from its ambient temperature (K or °C) and specific humidity (kg/kg) 
}
\usage{
TT.SH.2.TV(tt, sh, pp=1013.25)
}
\arguments{
  \item{tt}{ambient temperature (K >150 or °C<150)}
  \item{sh}{specific humidity (kg/kg)}
  \item{pp}{ambient pressure (hPa)}  
}
\details{
\code{TT.SH.2.TV} calculates the virtual temperature (K) of an air parcel from its ambient temperature (K or °C) and specific humidity (kg/kg)
}
\value{
    virtual temperature in units (K)
}
\author{Stehpan Henne}
\examples{
    
}
\keyword{manip}% at least one, from doc/KEYWORDS

