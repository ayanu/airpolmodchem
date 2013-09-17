\name{axis.chron}
\alias{axis.chron}
\title{ Plot axis with chron lables }
\description{
Plot's an axis using chron labels
}
\usage{
axis.chron(side, x, at, format, labels, tz = "GMT", ...)
}
\arguments{
  \item{side}{ (numeric) side on which to plot axis (1-4: lower, left, upper, right) }
  \item{x}{ (chron) optional, where to plot labels }
  \item{at}{ (chron) optional, where to plot labels, overwrites x }
  \item{format}{ format of labels, see \code{\link{strptime}} }
  \item{labels}{ (character) same length as 'at', giving the labels used at positions at, instead of automatic labels }
  \item{tz}{ timezone }
  \item{\dots}{ Further arguments passed to axis }
}
\value{
-
}
\author{ stephan.henne@empa.ch }
\seealso{ \code{\link{axis}}, \code{\link{axis.POSIXct}} }
\keyword{ manip }
