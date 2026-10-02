# Convert volume mixing ratios to number concentrations.

Convert volume mixing ratios in ppb to number concentrations 1/cm3.

## Usage

``` r
vmr.2.nc(vmr, TT = 20, pp = 1013.25)
```

## Arguments

- vmr:

  volume mixing ratio in ppb

- TT:

  ambient temperature in K, if \< 150 assumed to be in °C, default 20

- pp:

  ambient pressure in hPa, default 1013.25

## Value

Number concentration in units 1/cm3.

## Author

Stephan Henne (stephan.henne@emap.ch)

## See also

[`mc.2.vmr`](mc.2.vmr.md), [`vmr.2.mc`](vmr.2.mc.md)
