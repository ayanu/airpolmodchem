\name{TT_PP_2_PT}
\alias{TT_PP_2_PT}
\title{calculate potential temperature}
\description{
calculate potential temperature (K) from ambient temperature (K or °C) and ambient pressure (hPa)
}
\usage{
TT_PP_2_PT(tt, pp)
}
\arguments{
  \item{tt}{ambient temperatuer (K >150, °C <150}
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
print(TT_PP_2_PT(20, 1013.25))
print(TT_PP_2_PT(20, 800))
}
\keyword{manip}% at least one, from doc/KEYWORDS
