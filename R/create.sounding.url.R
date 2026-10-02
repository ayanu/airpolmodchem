# TODO: Add comment
# 
# Author: hes
###############################################################################

#' Creates a URL to retrieve radiosonde data 
#' 
#' Builds a URL to a specific radiosonde dataset as stored at the University of Wyoming radiosonde
#' archive. Soundings are usually done twice daily at 00 and 12 UTC. For availalbe station numbers
#' goto http://weather.uwyo.edu.
#' 
#' @param dtm (chron) time and date of sounding
#' @param stnm (character) station numver of sounding station. Payerne (CH): 06610
#' 
#' @return URL to individual sounding
#' 
# ' @example 
# ' 	require(chron)
# ' 	dtm=chron("2014-09-01", "12:00:00", format=c("y-m-d", "h:m:s"))
# ' 	url = create.sounding.url(dtm, stnm="06610")
# ' 
#' @author stephan.henne@@empa.ch
#' 
#' @export 
create.sounding.url = function(dtm, stnm){

  year = chron.2.string(dtm, "%Y")
	mon = chron.2.string(dtm, "%m")
	ddhh = chron.2.string(dtm, "%d%H")
	yymmdd = chron.2.string(dtm, "%Y-%m-%d")
	hhmmss = chron.2.string(dtm, "%H:%M:%S")

	url = paste0("https://weather.arcc.uwyo.edu/wsgi/sounding?datetime=", yymmdd, "%20", hhmmss, "&id=", 
	             stnm, "&type=TEXT:CSV&src=BUFR")

	return(url)
}


#require(chron)
#dtm=chron("2014-09-01", "12:00:00", format=c("y-m-d", "h:m:s"))
#url = create.sounding.url(dtm, stnm="06610")