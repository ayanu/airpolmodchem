# Calculate saturation vapor pressure above water

`ES` calculates the saturation vapor pressure above water from a given
temperature.

## Usage

``` r
ES(tt)
```

## Arguments

- tt:

  temperature of water (units interpreted as K if larger 150, otherwise
  degree C. Do not mix units in same vector.)

## Details

The formula 6.112 (hPa) \*exp(17.62\*tt / (243.12+tt)) is used if tt is
in (K).

## Value

the saturation vapor pressure in hPa

## Author

Stephan Henne <stephan.henne@empa.ch>

## See also

[`E.2.TD`](E.2.TD.md), [`ES.ice`](ES.ice.md)

## Examples

``` r
#   Saturation water vapor pressure at freezing point
    print(ES(273.15))   # assuming (K)
#> [1] 6.112
    print(ES(0))        # assuming (degree C)
#> [1] 6.112
```
