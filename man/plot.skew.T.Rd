\name{plot.skew.T}
\alias{plot.skew.T}
\title{Draw a Skew-T diagram}
\usage{
\method{plot}{skew.T}(snd, T.surf, qv.surf, plot.cape = TRUE,
  plot.LCL = TRUE, ...)
}
\arguments{
  \item{snd}{(data.frame) should contain at least the
  columns 'PRES' for atmospheric pressure in hPa, 'TEMP'
  for atmospheric temperature in °C, and 'MIXR' for the
  water vapour mixing ratio in g/kg.}

  \item{T.surf}{optional surface temperature (°C) used for
  LCL and CAPE calculation}

  \item{pv.surf}{optional water vapor mixing ratio at the
  surface (g/kg) used for LCL and CAPE calculation}

  \item{plot.LCL}{(logical) if TRUE lifting condensation
  level is calculated and displayed}

  \item{plot.cape}{(logical) if TRUE CAPE is calculated and
  displayed}
}
\value{
NULL at success
}
\description{
Draws a Skew-T diagram for a given atmsopheric profile of
atmospheric temperature, specific humidity and pressure.
Optionally calculates the Lifting Condensation Level and
Convective Available Potential Energy (CAPE).
}
\author{
stephan.henne@empa.ch
}
\references{
Rogers&Yau
}

