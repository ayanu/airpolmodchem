# calculate relative humidity from ambient and dew point temperature

`TT.TD.2.RH` calculates the relative humidity (%) of an air parcel from
its ambient temperature (K or °C) and dew point temperature (K or °C)

## Usage

``` r
TT.TD.2.RH(tt, td)
```

## Arguments

- tt:

  ambient temperature (K \>150 or °C\<150)

- td:

  dew point temperature (K \>150 or °C\<150)

## Details

`TT.TD.2.RH` calculates the relative humidity (%) of an air parcel from
its ambient temperature (K or °C) and dew point temperature (K or °C).

## Value

relative humidity (%)

## Author

Stehpan Henne

## Examples

``` r
    print(TT.TD.2.RH(20, 20))
#> [1] 100
    print(TT.TD.2.RH(20, 10))
#> [1] 52.56076
```
