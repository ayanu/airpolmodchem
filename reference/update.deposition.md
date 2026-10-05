# Calculate tendency due to deposition

Calculate tendency due to deposition

## Usage

``` r
# S3 method for class 'deposition'
update(spec, conc, vd = NULL, dt = 1, dz = 100)
```

## Arguments

- spec:

  vector of species to be treated

- conc:

  vector of concentrations from which to calculated the tendencies

- vd:

  vector of dry deposition velocities (units m s-1)

- dt:

  time step in seconds

- dz:

  Vertical height of surface layer in meters. Default is 100.

## Value

Concentration tendency due to dry deposition
