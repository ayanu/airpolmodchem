# Calculate saturation vapor pressure above ice

`ES.ice` calculates the saturation vapor pressure above ice from a given
temperature.

## Usage

``` r
ES.ice(tt)
```

## Arguments

- tt:

  temperature of ice (units interpreted as K if larger 150, otherwise
  °C. Do not mix units in the same vector.)

## Details

the formula 6.112 (hPa) \*exp(22.46\*tt/(272.62+tt)) is used if tt is in
(K)

## Value

the saturation vapor pressure in hPa

## Author

Stephan Henne <stephan.henne@empa.ch>

## See also

[`ES`](ES.md)

## Examples

``` r
#   should yield the same results
    print(ES.ice(273.15))   # assuming (K)
#> [1] 6.112
    print(ES.ice(0))        # assuming (°C)
#> [1] 6.112
```
