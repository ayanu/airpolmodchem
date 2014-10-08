#' Formats a chron object to character
#' 
#' Converts a chron object to a character representation. The same formatting specifiers are 
#' used as \code{\link{format.POSIXlt}}
#' 
#' @param dtm chron object
#' @param form format string, see \code{\link{format.POSIXlt}} for details
#' @param tz (character) giving name of time zone, see \code{\link{as.POSIXlt}}
#' 
#' @return formatted date/time string
#' 
#' @author stephan.henne@@empa.ch
#' 
#' @export
chron.2.string = function(dtm, form="%Y-%m-%d %H:%M:%S", tz="GMT"){
	return(format(as.POSIXlt(dtm, tz), form))
}
