\name{WS_WD_2_UU_VV}
\alias{WS_WD_2_UU_VV}
\title{Convert wind speed and direction into wind components}
\description{
\code{WS_WD_2_UU_VV} converts wind speed and wind direction into vector components.
}
\usage{
WS_WD_2_UU_VV(ws, wd)
}
\arguments{
  \item{ws}{wind speed}
  \item{wd}{wind direction (°)}
}
\value{
list
  \item{UU}{west-east wind component same units as ws}
  \item{VV}{south-north wind component same units as ws}
}
\author{Stephan Henne}
\seealso{\code{\link{UU_VV_2_WS_WD}}}
\examples{
print(UU_VV_2_WS_WD(10, 213))
}
\keyword{manip}% at least one, from doc/KEYWORDS
