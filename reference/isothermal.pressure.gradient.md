# Pressure decrease for isothermal layer

Calculates the pressure at an altitude increment assuming an isothermal
layer (constant temperature).

## Usage

``` r
isothermal.pressure.gradient(z, p0 = 1013.25, TT = 288.15)
```

## Arguments

- z:

  Altitude increment in units m

- p0:

  Pressure at bottom of layer

- TT:

  Mean temperature of layer in K or °C

## Details

Calculates the pressure at an altitude increment z assuming an
isothermal layer with temperature TT. The pressure at the bottom of the
layer needs to be given by p0. The result will have the same pressure
units as used for the input pressure p0.

## Value

Pressure at altitude z in units of p0.

## References

Uses isothermal formulation of the barometric formula.

## Author

stephan.henne@empa.ch

## See also

[`polytropic.pressure.gradient`](polytropic.pressure.gradient.md)
