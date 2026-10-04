# Pressure decrease for layer with constant pressure gradient

Calculates the pressure at an altitude increment for a layer with
constant temperature gradient.

## Usage

``` r
polytropic.pressure.gradient(z, p0 = 1013.25, T0 = 288.15, gamma = g.0/c.p)
```

## Arguments

- z:

  Altitude increment in units m

- p0:

  Pressure at bottom of layer.

- T0:

  Temperature at bottom of layer in K or degree C

- gamma:

  Gamma is the temperature lapse rate. By default dry adiabatic lapse
  rate for Earth atmosphere is used.

## Details

## Value

Pressure at incremented altitude in the same units as 'p0'.

## References

## Author

## Note

## See also

## Examples

``` r
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--  or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (z, p0 = 1013.25, T0 = 288.15, gamma = g.0/c.p) 
{
    T0[T0 < 100] = T0[T0 < 100] + 273.15
    p = p0 * ((T0 - gamma * z)/T0)^(g.0/R.air/gamma)
    return(p)
  }
#> function (z, p0 = 1013.25, T0 = 288.15, gamma = g.0/c.p) 
#> {
#>     T0[T0 < 100] = T0[T0 < 100] + 273.15
#>     p = p0 * ((T0 - gamma * z)/T0)^(g.0/R.air/gamma)
#>     return(p)
#> }
#> <environment: 0x55fe8f28eab0>
```
