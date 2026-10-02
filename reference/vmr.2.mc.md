# Convert volume mixing ratios to mass concentrations.

Convert volume mixing ratios in ppbv to mass concentrations in ug/cm3.

## Usage

``` r
vmr.2.mc(vmr, molw, species = "CO", TT = 20, pp = 1013.25)
```

## Arguments

- vmr:

  volume mixing ratio in ppb

- molw:

  molar mass of species under consideration in g/mol

- species:

  alternatively to molw the name of the species can be given. So far
  molar masses are included for: "CO", "CH4", "O3", "NO2", "NO", "N2O"

- TT:

  ambient temperature in °C, default 20s

- pp:

  ambient pressure in hPa, default 1013.25

## Value

Mass concentration in units ug/m3.

## Author

Stephan Henne (stephan.henne@emap.ch)

## See also

[`mc.2.vmr`](mc.2.vmr.md), [`vmr.2.nc`](vmr.2.nc.md)
