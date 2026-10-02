# Reads radiosonde data

The sounding data needs to be in the ASCII format as obtained from
University of Wyoming radiosonde archive. Only one sounding per file is
supported.

## Usage

``` r
get.sounding(fn)
```

## Arguments

- fn:

  (character) Either the filename or the URL of the sounding ASCII file.

## Value

data.frame with sounding data

## Author

stephan.henne@empa.ch
