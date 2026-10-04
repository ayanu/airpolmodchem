# Calculate planetary boundary layer height

Calculates the height of the planetary boundary layer height using a
bulk Richardson number method.

## Usage

``` r
bulk.richardson.cbl.height(
  snd,
  ri.cr = 0.25,
  plot = TRUE,
  unit = c("ASL", "AGL"),
  zlim = c(0, 5000),
  smooth = FALSE
)
```

## Arguments

- snd:

  data.frame with vertical sounding data. Needs to include the fields
  'pt', 'sh', 'uu', 'vv', and 'zz' for potential temperature, specific
  humidity, west-east wind speed, south-north wind speed and height,
  respectively.

- ri.cr:

  Critical bulk Richardson number. Altitude with Richardson numbers
  above this value are considered outside planetary boundary layer.

- plot:

  (logical) if TRUE (default) the vertical profiles are plotted and the
  estimated boundary layer height indicated

- unit:

  Unit of vertical coordinate. One of 'ASL' or 'AGL'.

- zlim:

  Vertical limits of profile plot.

- smooth:

  (logical) If TRUE, apply a 5-point running mean before calculating
  Richardson number. Useful for noisy sounding data. Default is FALSE:
  no running mean.

## Value

Planetary boundary layer height in units provided by 'unit'.
