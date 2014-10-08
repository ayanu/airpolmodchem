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
create.sounding.url = function(dtm,stnm){
		## '''
		## usage:  
		## create_sounding_url('30122010','00','06610')
		## 
		## input: 
		## date (str) - the date as ddmmyyyy
		## time (str) - the time as hh   ('00' or '12')
		## stnm (str) - the station number, e.g.: 
		## 10410 Essen (D)
		## 10618 Idar-Oberstein (D)
		## 07145 Trappes (F)
		## 06260 De Bilt (NL)
		## 10238 Bergen (D)
		## 06610 Payerne (CH)   
		## 
		## output: 
		## url (str) 
		## '''
	year = chron.2.string(dtm, "%Y")
	mon = chron.2.string(dtm, "%m")
	ddhh = chron.2.string(dtm, "%d%H")
	url = paste("http://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT", 
			"%3ALIST&YEAR=", year, "&MONTH=", mon, "&FROM=", ddhh, "&TO=", ddhh, "&STNM=", stnm, 
			sep="")
	
	return(url)
}


#require(chron)
#dtm=chron("2014-09-01", "12:00:00", format=c("y-m-d", "h:m:s"))
#url = create.sounding.url(dtm, stnm="06610")