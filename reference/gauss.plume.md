# Gauss plume calculation

Calculates concentration of a Gaussian plume for a given location
(x,y,z), assuming that the source is located at x=0, y=0 and z=h.e and
main wind only in x direction. Stability classes following the Pasquill
definition are used.

## Usage

``` r
gauss.plume(x, y, z, Q, h.e, u, stability = "D", h.m = NULL)
```

## Arguments

- x:

  Location in x direction for which to calculate the Gauss plume
  concentration, given in m from source. Note that x,y,z can be arrays,
  but they should be of the same shape.

- y:

  Location in y direction for which to calculate the Gauss plume
  concentration, given in m from source.

- z:

  Location in z direction for which to calculate the Gauss plume
  concentration, given in m above ground.

- Q:

  emission flux of the source. Given in kg/s.

- h.e:

  effective emission height. Given in m above ground.

- u:

  wind speed in main wind direction (x) given in m/s.

- stability:

  Stability class following Pasquill definition. Possible values: A:
  extremely unstable B: moderately unstable C: slightly unstable D:
  neutral E: slightly stable F: stable

- h.m:

  Boundary layer height given in m above ground. If missing or NA no
  boundary layer top is assumed.

## Value

Concentration resulting from source strength Q, given in kg/m3
