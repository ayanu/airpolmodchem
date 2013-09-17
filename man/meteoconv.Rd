\name{meteoconv}
\alias{meteoconv}
\title{Meteorological Conversion Functions}
\description{
  \code{meteoconv} contains functions that 
  \itemize{
  	\item convert between different meteorologically relevant quantities
  	\item provide bulk calculations of meteorological quantities
  	\item perform simple meteorology specific tasks
  }
}
\details{

Conversion functions
	\describe{
  \item{\code{\link{mc2vmr}}}{Convert mass concentrations to volume mixing ratios.}
  \item{\code{\link{RH_TT_2_E}}}{Calculate water vapor partial pressure of an air parcel}
  \item{\code{\link{RH_TT_2_TD}}}{Calculate dew point temperature of an air parcel}
  \item{\code{\link{RH_TT_PP_2_SH}}}{Calculate specific humidity from relative humidity, ambient temperature, and pressure}
  \item{\code{\link{TT_PP_2_PT}}}{Calculate potential temperature}
  \item{\code{\link{TT_TD_2_RH}}}{Calculate relative humidity from ambient and dew point temperature}
  \item{\code{\link{UU_VV_2_WS_WD}}}{Converts wind components to wind speed and direction}
  \item{\code{\link{vmr2mc}}}{Convert volume mixing ratios to mass concentrations.}
  \item{\code{\link{vmr2nc}}}{Convert volume mixing ratios to number concentrations.}
  \item{\code{\link{WS_WD_2_UU_VV}}}{Convert wind speed and direction into wind components}
  \item{\code{\link{WS_WD_angle_2_UU_VV}}}{Convert wind speed and direction into wind components parallel and perpendicular to a specified direction}
}
Bulk calculations of meteorological quantities
	\describe{
  \item{\code{\link{ES.ice}}}{Calculate saturation vapor pressure above ice}
  \item{\code{\link{ES.inv}}}{Calculate temperature of water surface for given saturation water vapor pressure}
  \item{\code{\link{ES}}}{Calculate saturation vapor pressure above water}  
  \item{\code{\link{global.radiation}}}{Calculate potential global radiation}
  \item{\code{\link{solar.zenith.angle}}}{Calculate solor zenith angle }
}
Simple tools and plots


}
\author{
  Stephan Henne (stephan@emap.ch)
}
\keyword{manip}

