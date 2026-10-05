# Simple time-series plot

Plots one or more variables of the passed data.frame against time.

## Usage

``` r
ts.plot(
  X,
  para = names(X)[2],
  xlim,
  ylim = NULL,
  xlab,
  ylab = para,
  pch,
  lty,
  col,
  leg.pos = "topleft",
  stacked = 0,
  accumulated = FALSE,
  legend,
  dtm.col = "dtm",
  flag = NULL,
  bg.flag = NULL,
  col.par,
  color.palette = rainbow,
  scale = NULL,
  ...
)
```

## Arguments

- X:

  data.frame containing time-series data

- para:

  parameter(s)/column(s) to plot

- xlim:

  2-element vector giving the limits of the x-axis. If missing
  determined from X.

- ylim:

  2-element vector giving the limits of the y-axis. Default NULL:
  determine from data.

- xlab:

  Label for the x-axis. Missing by default: Using time range as axis
  label.

- ylab:

  Label for y-axis. Default is to use 'para'.

- pch:

  symbol index to be used for the plot; if missing set automatically

- lty:

  line type to be used for the plot; if missing set automatically

- col:

  color to be used for the plot; if missing set automatically

- leg.pos:

  Legend position. Passed to call to 'legend'. Default: 'topleft'

- stacked:

  Default (0) is to plot different parameters/columns in same plot with
  single y-axis. But giving a positive number to stacked, multiple
  parameters will be plotted on top of each other in separate panels.
  The number determines how many parameters are plotted per sub-plot.

- accumulated:

  If TRUE the sum of all parameters is formed and stacked surfaces are
  plotted instead of lines/symbols. Requires all variables to either be
  positive or negative.

- legend:

  Alternative legend text.

- dtm.col:

  Name of column in 'X' that contains the time variable. Default 'dtm'.

- flag:

  Name of column that contains flagging data. Differently flagged data
  points will be plotted with different symbols/colors.

- bg.flag:

  Value of special flag for background data.

- col.par:

  Additional secondary parameter that can be used for coloring time
  series.

- color.palette:

  Color palette function used for coloring time series.

- scale:

  Additional scaling factor for all time series data. Default is NULL:
  no scaling.

- ...:

  Additional argument passed to other routines.
