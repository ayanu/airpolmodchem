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
cat("Relative humidity: 80 \n")
#> Relative humidity: 80 
cat("Temperature: 20°C\n")
#> Temperature: 20°C
cat("Dewpoint temperature:", RH.TT.2.TD(80, 20), "°C\n")
#> Dewpoint temperature: 289.5924 °C
```
