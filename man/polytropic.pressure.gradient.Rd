\name{polytropic.pressure.gradient}
\alias{polytropic.pressure.gradient}
\title{
Pressure decrease for layer with constant pressure gradient
}
\description{
Calculates the pressure at an altitude increment for a layer with constant temperature gradient.
}
\usage{
polytropic.pressure.gradient(z, p0 = 1013.25, T0 = 288.15, gamma = g.0/c.p)
}
\arguments{
  \item{z}{
  Altitude increment in units m
}
  \item{p0}{
Pressure at bottom of layer 
}
  \item{T0}{
Temperature at bottom of layer in K or °C
}
  \item{gamma}{
%%     ~~Describe \code{gamma} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (z, p0 = 1013.25, T0 = 288.15, gamma = g.0/c.p) 
{
    T0[T0 < 100] = T0[T0 < 100] + 273.15
    p = p0 * ((T0 - gamma * z)/T0)^(g.0/R.air/gamma)
    return(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
