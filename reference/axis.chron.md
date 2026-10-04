# Add axis with date/time labels for chron dimension

Adds axis in same way as call to 'axis'. Ticks and labels will be pretty
date/time.

## Usage

``` r
axis.chron(
  side,
  x,
  at,
  format,
  labels,
  tz = "GMT",
  lwd = par("lwd"),
  lwd.ticks = par("lwd"),
  ...
)
```

## Arguments

- side:

  an integer specifying which side of the plot the axis is to be drawn
  on. The axis is placed as follows: 1=below, 2=left, 3=above and
  4=right.

- x:

  Alternative values of where axis ticks should be drawn.

- at:

  the points at which tick-marks are to be drawn. Non-finite (infinite,
  ‘NaN’ or ‘NA’) values are omitted. By default (when ‘NULL’) tickmark
  locations are computed, see ‘Details’ below.

- format:

  A format string for the conversion of chron to string. See '?strptime'
  for details.

- labels:

  this can either be a logical value specifying whether (numerical)
  annotations are to be made at the tickmarks, or a character or
  expression vector of labels to be placed at the tick points. (Other
  objects are coerced by ‘as.graphicsAnnot’.) If this is not logical,
  ‘at’ should also be supplied and of the same length. If ‘labels’ is of
  length zero after coercion, it has the same effect as supplying
  ‘TRUE’.

- tz:

  time zone of chron object. Default is "GMT".

- lwd:

  line width of axis line

- lwd.ticks:

  line width of tick marks

- ...:

  other parameters passed to 'axis'

## Value

location on tick marks
