\name{vmr.2.mc}
\alias{vmr.2.mc}
\title{Convert volume mixing ratios to mass concentrations.}
\description{
Convert volume mixing ratios in ppbv to mass concentrations in ug/cm3. 
}
\usage{
vmr.2.mc(vmr, molw, species = "CO", TT = 20, pp = 1013.25)
}
\arguments{
  \item{vmr}{ volume mixing ratio in ppb }
  \item{molw}{molar mass of species under consideration in g/mol }
  \item{species}{ alternatively to molw the name of the species can be given. So far molar masses are included for: "CO", "CH4", "O3", "NO2", "NO", "N2O"  }
  \item{TT}{ ambient temperature in °C, default 20s }
  \item{pp}{ ambient pressure in hPa, default 1013.25 }
}
\value{
    Mass concentration in units ug/m3. 
}
\author{ Stephan Henne (stephan.henne@emap.ch) }
\seealso{ \code{\link{mc.2.vmr}}, \code{\link{vmr.2.nc}} }
\keyword{ manip }

