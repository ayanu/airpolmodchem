\name{vmr2mc}
\alias{vmr2mc}
\title{Convert volume mixing ratios to mass concentrations.}
\description{
Convert volume mixing ratios in ppbv to mass concentrations in ug/cm3. 
}
\usage{
vmr2mc(vf, molw, species = "CO", TT = 20, pp = 1013.25)
}
\arguments{
  \item{vf}{ volume mixing ratio in ppb }
  \item{molw}{molweight of species under consideration in g/mol }
  \item{species}{ alternatively to molw the name of the species can be given. So far molweights are included for: "CO", "CH4", "O3", "NO2", "NO", "N2O"  }
  \item{TT}{ ambient temperature in °C, default 20 }
  \item{pp}{ ambient pressure in hPa, default 1000 }
}
\value{
    Mass concentration ug/m3 
}
\author{ Stephan Henne (stephan.henne@emap.ch) }
\seealso{ \code{\link{mc2vmr}}, \code{\link{vmr2nc}} }
\keyword{ manip }

