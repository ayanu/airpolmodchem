# Creates a URL to retrieve radiosonde data

Builds a URL to a specific radiosonde dataset as stored at the
University of Wyoming radiosonde archive. Soundings are usually done
twice daily at 00 and 12 UTC. For availalbe station numbers goto
http://weather.uwyo.edu.

## Usage

``` r
create.sounding.url(dtm, stnm)
```

## Arguments

- dtm:

  (chron) time and date of sounding

- stnm:

  (character) station numver of sounding station. Payerne (CH): 06610

## Value

URL to individual sounding

## Author

stephan.henne@empa.ch
