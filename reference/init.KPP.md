# Initialise KPP

Initial values for chemical compounds, time step, temperature and other
global variables.

## Usage

``` r
init.KPP(init.var = NULL, var.default = 0, dt, tt = 270, init.gl = NULL)
```

## Arguments

- init.var:

  (list) initial values of all species

- var.default:

  (numeric) inital value for species not in init.var

- dt:

  (numeric) time step in seconds

- tt:

  (numeric) temperature in K

- init.gl:

  (list) initial values of user defined globals
