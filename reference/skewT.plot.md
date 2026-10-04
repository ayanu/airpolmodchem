# Draw a Skew-T diagram

Draws a Skew-T diagram for a given atmsopheric profile of atmospheric
temperature, specific humidity and pressure. Optionally calculates the
Lifting Condensation Level and Convective Available Potential Energy
(CAPE).

## Usage

``` r
skewT.plot(
  snd,
  T.surf,
  qv.surf,
  plot = TRUE,
  plot.cape = TRUE,
  plot.LCL = TRUE,
  dp = 100,
  ...
)
```

## Arguments

- snd:

  (data.frame) should contain at least the columns 'PRES' for
  atmospheric pressure in hPa, 'TEMP' for atmospheric temperature in
  degree C, and 'MIXR' for the water vapour mixing ratio in g/kg.

- T.surf:

  optional surface temperature (degree C) used for LCL and CAPE
  calculation

- qv.surf:

  optional water vapor mixing ratio at the surface (g/kg) used for LCL
  and CAPE calculation.

- plot:

  (logical) If TRUE (default) sounding and parcle descent are plotted.
  Otherwise only result values are returned.

- plot.cape:

  (logical) if TRUE CAPE is calculated and displayed

- plot.LCL:

  (logical) if TRUE lifting condensation level is calculated and
  displayed

- dp:

  Vertical step for calculating parcel ascend. In units Pa. Default is
  100.

- ...:

  Additional arguments passed to 'plot.skewT.ax'

## Value

list containing values for lifting condensation level and CAPE (if
requested)

## References

Rogers&Yau

## Author

stephan.henne@empa.ch
