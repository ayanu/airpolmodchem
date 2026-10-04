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
cat("TT=20°C, PP=1013.25 hPa -> PT=,", TT.PP.2.PT(20, 1013.25), "\n")
#> TT=20°C, PP=1013.25 hPa -> PT=, 292.0501 
cat("TT=20°C, PP=800 hPa -> PT=,", TT.PP.2.PT(20, 800), "\n")
#> TT=20°C, PP=800 hPa -> PT=, 312.4386 
```
