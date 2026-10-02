# calculate potential temperature

calculate potential temperature (K) from ambient temperature (K or °C)
and ambient pressure (hPa)

## Usage

``` r
TT.PP.2.PT(tt, pp)
```

## Arguments

- tt:

  ambient temperatuer (K \>150, °C \<150)

- pp:

  ambient pressure (hPa)

## Details

Calculate potential temperature (K) of an air parcel from its ambient
temperature (K or °C) and its ambient pressure (hPa).

## Value

Potential temperature (K)

## Author

Stephan Henne

## Examples

``` r
print(TT_PP_2_PT(20, 1013.25))
#> Error in TT_PP_2_PT(20, 1013.25): could not find function "TT_PP_2_PT"
print(TT_PP_2_PT(20, 800))
#> Error in TT_PP_2_PT(20, 800): could not find function "TT_PP_2_PT"
```
