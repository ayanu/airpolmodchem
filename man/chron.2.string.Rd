\name{chron.2.string}
\alias{chron.2.string}
\title{Formats a chron object to character}
\usage{
chron.2.string(dtm, form = "\%Y-\%m-\%d \%H:\%M:\%S", tz = "GMT")
}
\arguments{
  \item{dtm}{chron object}

  \item{form}{format string, see
  \code{\link{format.POSIXlt}} for details}

  \item{tz}{(character) giving name of time zone, see
  \code{\link{as.POSIXlt}}}
}
\value{
formatted date/time string
}
\description{
Converts a chron object to a character representation. The
same formatting specifiers are used as
\code{\link{format.POSIXlt}}
}
\author{
stephan.henne@empa.ch
}

