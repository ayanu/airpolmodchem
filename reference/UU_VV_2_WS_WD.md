# Convert horizontal wind vector components to wind spped and direction

Units for wind components 'uu' and 'vv' have to be the same. Units of
returned wind speed will be accordingly. 'uu' and 'vv' can be vectors or
arrays, but have to be of the same shape.

## Usage

``` r
UU_VV_2_WS_WD(uu, vv)
```

## Arguments

- uu:

  wind component in west-east direction

- vv:

  wind component in south-north direction

## Value

list of WS (wind speed) and WD (wind direction). Units of WS will be the
same as 'uu' and 'vv'.
