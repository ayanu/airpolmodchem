# Draw a Skew-T diagram

Draws a Skew-T diagram for a given atmsopheric profile of atmospheric
temperature, specific humidity and pressure. Optionally calculates the
Lifting Condensation Level and Convective Available Potential Energy
(CAPE).

## Usage

``` r
# S3 method for class 'skew.T'
plot(snd, T.surf, qv.surf, plot.cape = TRUE,
  plot.LCL = TRUE, ...)
```

## Arguments

- snd:

  (data.frame) should contain at least the columns 'PRES' for
  atmospheric pressure in hPa, 'TEMP' for atmospheric temperature in
  degree C, and 'MIXR' for the water vapour mixing ratio in g/kg.

- T.surf:

  optional surface temperature (degree C) used for LCL and CAPE
  calculation

- pv.surf:

  optional water vapor mixing ratio at the surface (g/kg) used for LCL
  and CAPE calculation

- plot.LCL:

  (logical) if TRUE lifting condensation level is calculated and
  displayed

- plot.cape:

  (logical) if TRUE CAPE is calculated and displayed

## Value

NULL at success

## Author

stephan.henne@empa.ch

## References

Rogers&Yau
