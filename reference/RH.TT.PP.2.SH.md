# Calculate specific humidity from relative humidity, ambient temperature, and pressure

`RH.TT.PP.2.SH` calculates the specific humidity (kg/kg) of an air
parcel from its relative humidity (%), temperature (K or °C) and
pressure (hPa).

## Usage

``` r
RH.TT.PP.2.SH(rh, tt, pp)
```

## Arguments

- rh:

  relative humidity of air parcel (%)

- tt:

  temperature of air parcel (K (\>150) or °C (\<150)

- pp:

  ambient pressure (hPa)

## Value

specific humidity of air parcel in kg water per kg air (kg/kg)

## Author

Stephan Henne <stephan.henne@empa.ch>

## Examples

``` r
## 
print(RH.TT.PP.2.SH(100, 10, 1000))
#> [1] 0.007661414
print(RH.TT.PP.2.SH(100, 25, 1000))
#> [1] 0.01989318
```
