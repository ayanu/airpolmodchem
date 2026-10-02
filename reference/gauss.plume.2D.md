# Gauss plume calculation (directional)

Calculates concentration of a Gaussian plume for a given location
(x,y,z), assuming that the source is located at x=0, y=0 and z=h.e. Main
wind direction can be given. Stability classes following the Pasquill
definition are used.

## Usage

``` r
gauss.plume.2D(u = 1, v = 1, WS, WD, x, y, z, Q, h.e, stability, h.m)
```

## Arguments

- u:

  wind speed in x direction given in m/s.

- v:

  wind speed in x direction given in m/s.

- WS:

  scalar wind speed given in m/s.

- WD:

  wind direction given in degree. 0: wind from north, 90: wind from
  east, 180: wind from south, 270: wind from west.

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

- stability:

  Stability class following Pasquill definition. Possible values: A:
  extremely unstable B: moderately unstable C: slightly unstable D:
  neutral E: slightly stable F: stable

- h.m:

  Boundary layer height given in m above ground. If missing or NA no
  boundary layer top is assumed.

## Value

Concentration resulting from source strength Q, given in kg/m3
