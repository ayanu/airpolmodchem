\name{mc2vmr}
\alias{mc2vmr}
\title{Convert mass concentrations to volume mixing ratios.}
\description{
Convert number concentrations given in ug/cm3 to volume mixing ratios in ppbv. 
}
\usage{
mc2vmr(cc, molw, species = "CO", TT = 20, pp = 1013.25)
}
\arguments{
  \item{cc}{ mass concentration in ug/m3 }
  \item{molw}{molweight of species under consideration in g/mol }
  \item{species}{ alternatively to molw the name of the species can be given. So far molweights are included for: "CO", "CH4", "O3", "NO2", "NO", "N2O"  }
  \item{TT}{ ambient temperature in °C, default 20 }
  \item{pp}{ ambient pressure in hPa, default 1000 }
}
\value{
    Volume mixing ratio in ppb. 
}
\author{ Stephan Henne (stephan.henne@emap.ch) }
\seealso{ \code{\link{vmr2mc}}, \code{\link{vmr2nc}} }
\keyword{ manip }

