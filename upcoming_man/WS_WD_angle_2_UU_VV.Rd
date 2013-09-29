\name{WS_WD_angle_2_UU_VV}
\alias{WS_WD_angle_2_UU_VV}
\title{Convert wind speed and direction into wind components parallel and perpendicular to a specified direction}
\description{
\code{WS_WD_angle_2_UU_VV} converts wind speed and wind direction into vector components.
}
\usage{
WS_WD_angle_2_UU_VV(ws, wd, angle)
}
\arguments{
  \item{ws}{wind speed}
  \item{wd}{wind direction (°)}
  \item{angle}{angle of (for example) downslope wind direction (°)}
}
\value{
list
  \item{UU}{wind speed perpendicular to given direction same units as ws}
  \item{VV}{wind speed parallel to given direction (negative for given direction) same units as ws}
}
\author{Stephan Henne}
\seealso{\code{\link{WS_WD_2_UU_VV}}}
\keyword{manip}
