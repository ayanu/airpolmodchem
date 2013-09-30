\name{AirPolModChem-package}
\alias{AirPolModChem-package}
\alias{AirPolModChem}
\docType{package}
\title{
Functions for ETH Lecture Air Pollution Modeling and Chemistry
}
\description{
A collection of useful functions and simple model tools as discussed during the ETH lecture Air Pollution Modeling and Chemistry in HS2013.
}
\details{
\tabular{ll}{
Package: \tab AirPolModChem\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2013-09-16\cr
Depends: \tab R (>= 3.0.0), chron\cr
License: \tab GPL (>=3)\cr
Encoding: \tab ISO-8859-1\cr
Built: \tab R 3.0.1; x86_64-w64-mingw32; 2013-09-29 19:49:05 UTC; windows\cr
}

\preformatted{
AirPolModChem-package   Functions for ETH Lecture Air Pollution
                        Modeling and Chemistry
E.2.TD                  Calculate temperature of water surface for
                        given a certain water vapor pressure
E.2.TD.ice              Calculate temperature of ice surface given a
                        certain water vapor pressure
ES                      Calculate saturation vapor pressure above water
ES.ice                  Calculate saturation vapor pressure above ice
RH.TT.2.E               Calculate water vapor partial pressure of an
                        air parcel
RH.TT.2.TD              Calculate dew point temperature of an air
                        parcel
RH.TT.PP.2.SH           Calculate specific humidity from relative
                        humidity, ambient temperature, and pressure
TT.PP.2.PT              calculate potential temperature
TT.SH.2.TV              calculate virtual temperature from temperature
                        and specific humidity
TT.TD.2.RH              calculate relative humidity from ambient and
                        dew point temperature
TT.TD.PP.2.TV           calculate virtual temperature from ambient and
                        dew point temperature
constants               Physical constants
isa.pp.2.zz             Pressure to altitude for international standard
                        atmosphere
isa.zz.2.pp             Altitude to pressure for international standard
                        atmosphere
isa.zz.2.tt             Altitude to pressure for international standard
                        atmosphere
mc.2.vmr                Convert mass concentrations to volume mixing
                        ratios.
nc.2.vmr				Convert number concentrations to volume mixng
						ratios.                        
vmr.2.mc                Convert volume mixing ratios to mass
                        concentrations.
vmr.2.nc                Convert volume mixing ratios to number
                        concentrations.
}
}

\author{
Stephan Henne <stephan.henne@empa.ch>

Maintainer: Stephan Henne <stephan.henne@empa.ch>
}

\keyword{ package }

