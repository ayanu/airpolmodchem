# Reads radiosonde data

The sounding data needs to be in the csv format as obtained from
University of Wyoming radiosonde archive. Only one sounding per file is
supported. Check: https://weather.arcc.uwyo.edu/upperair/sounding.shtml
Select 'Output type': Comma Separated Values

## Usage

``` r
get.sounding(url)
```

## Arguments

- url:

  (character) Either the filename or the URL of the sounding csv file.

## Value

data.frame with sounding data

## Author

stephan.henne@empa.ch
