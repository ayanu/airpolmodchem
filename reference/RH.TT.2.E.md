# Calculate water vapor partial pressure of an air parcel

Calculate water vapor partial pressure of an air parcel given in (hPa)
from the air parcel's relatve humidity (%) and its ambient temperature
(K or °C).

## Usage

``` r
RH.TT.2.E(rh, tt)
```

## Arguments

- rh:

  relative humidity (%)

- tt:

  ambient temperature (K if \>150, °C if \<150)

## Value

water vapor partial pressure (hPa)

## Author

Stephan Henne

## Examples

``` r
##   Water vapor partial pressure at 20°C and 100 relative humidity
print(RH.TT.2.E(100, 20))
#> [1] 23.32596
```
