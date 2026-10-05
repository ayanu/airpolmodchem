# Convert horizontal wind speed and direction into vector components

Units of returned wind components will be the same as for passed wind
speed 'ws'. 'ws' and 'wd' can be vectors or arrays, but have to be of
the same shape.

## Usage

``` r
WS_WD_2_UU_VV(ws, wd)
```

## Arguments

- ws:

  wind speed

- wd:

  wind direction

## Value

list of 'uu' (west-east wind component) and 'vv' (south-north wind
component). Units of WS will be the same as 'ws'.
