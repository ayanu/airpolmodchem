# Calculate dew point temperature of an air parcel

Calculate dew point temperature of an air parcel (K) from the air
parcel's relatve humidity (%) and its ambient temperature (K or °C).

## Usage

``` r
RH.TT.2.TD(rh, tt)
```

## Arguments

- rh:

  relative humidity (%)

- tt:

  ambient temperature (K if \>150, °C if \<150)

## Value

dew point temperature (K)

## Author

Stephan Henne

## Examples

``` r
## 
print(RH_TT_2_TD(100, 20))
#> Error in RH_TT_2_TD(100, 20): could not find function "RH_TT_2_TD"
```
