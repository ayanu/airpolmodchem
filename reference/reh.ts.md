# Time series of observed meteorology and stability category

Time series of meteorological observations and stability categories for
the MeteoSwiss site Reckenholz with hourly resolution and for the year
2015.

## Usage

``` r
data(reh.ts)
```

## Format

data.frame containing fields:

- dtm:

  date/time chron object

- WS:

  Wind speed in units m s-1.

- WD:

  Wind direction, central value.

- stability:

  Pasquill stability category

- TT:

  2m ambient temperature in degree C

- RH:

  2m relative humidity in %
