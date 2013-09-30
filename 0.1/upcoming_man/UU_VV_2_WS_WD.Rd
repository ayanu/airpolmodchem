\name{UU_VV_2_WS_WD}
\alias{UU_VV_2_WS_WD}
\title{Converts wind components to wind speed and direction}
\description{
\code{UU_VV_2_WS_WD} converts horizontal wind information given as components to wind speed and direction.
}
\usage{
UU_VV_2_WS_WD(uu, vv)
}
\arguments{
  \item{uu}{west-east wind component}
  \item{vv}{south-north wind component}
}
\details{
uu and vv have to be given with the same units
}
\value{
 list
  \item{WS}{Wind speed with same units as input}
  \item{WD}{Wind direction (°)}
}
\author{Stephan Henne}
\seealso{\code{\link{WS_WD_2_UU_VV}}}
\examples{
    print(UU_VV_2_WS_WD(10, 10))
}
\keyword{manip}% at least one, from doc/KEYWORDS
