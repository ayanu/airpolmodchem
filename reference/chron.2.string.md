# Formats a chron object to character

Converts a chron object to a character representation. The same
formatting specifiers are used as
[`format.POSIXlt`](https://rdrr.io/r/base/strptime.html)

## Usage

``` r
chron.2.string(dtm, form = "%Y-%m-%d %H:%M:%S", tz = "GMT")
```

## Arguments

- dtm:

  chron object

- form:

  format string, see
  [`format.POSIXlt`](https://rdrr.io/r/base/strptime.html) for details

- tz:

  (character) giving name of time zone, see
  [`as.POSIXlt`](https://rdrr.io/r/base/as.POSIXlt.html)

## Value

formatted date/time string

## Author

stephan.henne@empa.ch
