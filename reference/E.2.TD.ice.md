# Calculate dew point temperature for deposition (freezing) for a given water vapor pressure

Calculate dew point temperature for deposition (freezing) for a given
water vapor pressure.

## Usage

``` r
E.2.TD.ice(e)
```

## Arguments

- e:

  water vapor pressure (hPa)

## Details

Calculates the temperature (K) of an ice surface for which at a given
water vapor pressure (hPa) freezing (deposition) would set in.

## Value

deposition (freezing) point temperature (K)

## Author

Stephan Henne <stephan.henne@empa.ch>

## See also

[`ES.ice`](ES.ice.md)

## Examples

``` r
#   get water surface temperature at 20 hPa
    print(E.2.TD.ice(20))
#> [1] 288.3412
```
