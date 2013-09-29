\name{mc.2.vmr}
\alias{mc.2.vmr}
\title{Convert mass concentrations to volume mixing ratios.}
\description{
Convert mass concentrations given in ug/m3 to volume mixing ratios in ppbv. 
}
\usage{
mc.2.vmr(cc, molw, species = "CO", TT = 20, pp = 1013.25)
}
\arguments{
  \item{cc}{ mass concentration in ug/m3 }
  \item{molw}{ molar mass of species under consideration in g/mol }
  \item{species}{ alternatively to molw the name of the species can be given. So far molar masses are included for: "CO", "CH4", "O3", "NO2", "NO", "N2O"  }
  \item{TT}{ ambient temperature in °C, default 20 }
  \item{pp}{ ambient pressure in hPa, default 1013.25 }
}
\value{
    Volume mixing ratio (mole fraction) in ppb. 
}
\author{ Stephan Henne (stephan.henne@emap.ch) }
\seealso{ \code{\link{vmr.2.mc}}, \code{\link{vmr.2.nc}} }
\keyword{ manip }

