\name{isa.pp.2.zz}
\alias{isa.pp.2.zz}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Pressure to altitude for international standard atmosphere }
\description{
    Estimates the altitude above sea level for a given pressure level and international standard atmospheric conditions.
}
\usage{
isa.pp.2.zz(pp = 101325)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pp}{ (numeric) pressure level in [Pa] }
}

\value{
    (numeric) altitude above sea level in [m]
}
\references{ Compare http://en.wikipedia.org/wiki/International\_Standard\_Atmosphere and Etling, D.: 1996. Theoretische Meteorologie. }
\author{ stephan.henne@empa.ch }
\seealso{ \code{\link{isa.zz.2.pp}}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

