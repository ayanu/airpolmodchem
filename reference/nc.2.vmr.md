# Convert number concentrations to volume mixing ratios.

Convert number concentrations 1/cm3 to volume mixing ratios in ppb .

## Usage

``` r
nc.2.vmr(nc, TT = 20, pp = 1013.25)
```

## Arguments

- nc:

  Number concentration in units 1/cm3

- TT:

  ambient temperature in K, if \< 150 assumed to be in °C, default 20

- pp:

  ambient pressure in hPa, default 1013.25

## Value

volume mixing ratio in ppb

## Author

Stephan Henne (stephan.henne@emap.ch)

## See also

[`mc.2.vmr`](mc.2.vmr.md), [`vmr.2.mc`](vmr.2.mc.md)
