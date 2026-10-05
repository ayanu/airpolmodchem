# Frequency of dispersion category

Frequency of wind-speed, wind-direction, stability categories for the
MeteoSwiss site Reckenholz and the year 2015.

## Usage

``` r
data(reh.freq)
```

## Format

data.frame containing fields:

- freq:

  frequency of dispersion category

- WS:

  Wind speed, central value in units m s-1.

- WD:

  Wind direction, central value.

- stability:

  Pasquill stability category

- h.m:

  mixing layer height; all values NA as not measured; can be set for
  supplying data.frame to 'average.gauss.plume.from.freq'
