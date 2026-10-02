# Convert mass concentrations to volume mixing ratios.

Convert mass concentrations given in ug/m3 to volume mixing ratios in
ppbv.

## Usage

``` r
mc.2.vmr(cc, molw, species = "CO", TT = 20, pp = 1013.25)
```

## Arguments

- cc:

  mass concentration in ug/m3

- molw:

  molar mass of species under consideration in g/mol

- species:

  alternatively to molw the name of the species can be given. So far
  molar masses are included for: "CO", "CH4", "O3", "NO2", "NO", "N2O"

- TT:

  ambient temperature in °C, default 20

- pp:

  ambient pressure in hPa, default 1013.25

## Value

Volume mixing ratio (mole fraction) in ppb.

## Author

Stephan Henne (stephan.henne@emap.ch)

## See also

[`vmr.2.mc`](vmr.2.mc.md), [`vmr.2.nc`](vmr.2.nc.md)
