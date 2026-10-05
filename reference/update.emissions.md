# Calculate tendency due to emissions

Calculate tendency due to emissions

## Usage

``` r
# S3 method for class 'emissions'
update(
  spec,
  E = NULL,
  mu = NULL,
  dt = 1,
  V = 1,
  dtm = chron(0),
  hour.profile = NULL
)
```

## Arguments

- spec:

  vector of species to be treated

- E:

  (list) emissions by name, units: kg/s

- mu:

  (list) molar mass by name, units: g/mole

- dt:

  (numeric) time step, units: s

- V:

  (numeric) volume of box, units: m3

- dtm:

  date/time (chron)

- hour.profile:

  time of day profile of emission scaling factors

## Value

Concentration tendency due to dry deposition
