\name{TT.PP.2.PT}
\alias{TT.PP.2.PT}
\title{calculate potential temperature}
\description{
calculate potential temperature (K) from ambient temperature (K or °C) and ambient pressure (hPa)
}
\usage{
TT.PP.2.PT(tt, pp)
}
\arguments{
  \item{tt}{ambient temperatuer (K >150, °C <150) }
  \item{pp}{ambient pressure (hPa)}
}
\details{
Calculate potential temperature (K) of an air parcel from its ambient temperature (K or °C) and its ambient pressure (hPa).
}
\value{
Potential temperature (K)
}
\author{Stephan Henne}
\examples{
cat("TT=20°C, PP=1013.25 hPa -> PT=,", TT.PP.2.PT(20, 1013.25), "\n")
cat("TT=20°C, PP=800 hPa -> PT=,", TT.PP.2.PT(20, 800), "\n")
}
\keyword{manip}% at least one, from doc/KEYWORDS
