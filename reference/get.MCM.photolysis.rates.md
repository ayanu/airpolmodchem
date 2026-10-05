# Calculate photolysis rates

Applies the MCM/CRI parameterisation of photolysis rates based on
location and time only. A total of 61 photolyiss rates are calculated.
Refer to MCM/CRI for details.

## Usage

``` r
get.MCM.photolysis.rates(dtm, lon = 0, lat = 0)
```

## Arguments

- dtm:

  (chron) single date/time

- lon:

  (numeric) longitude in degrees east

- lat:

  (numeric) latitude in degrees north

## Value

vector of photolysis rates using the MCM photolysis reaction
definitions.
