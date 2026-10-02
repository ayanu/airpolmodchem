# Calculate dew point temperature for condensation for a given water vapor pressure

Calculate dew point temperature for condensation for a given water vapor
pressure.

## Usage

``` r
E.2.TD(e)
```

## Arguments

- e:

  water vapor pressure (hPa)

## Details

Calculates the temperature (K) for which at a given water vapor pressure
(hPa) condensation would set in.

## Value

dew point temperature (K)

## Author

Stephan Henne <stephan.henne@empa.ch>

## See also

[`ES`](ES.md)

## Examples

``` r
#   get water surface temperature at 20 hPa
    print(E.2.TD(20))
#> [1] 290.6871
```
