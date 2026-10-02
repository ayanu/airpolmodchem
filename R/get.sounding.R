# TODO: Add comment
# 
# Author: hes
###############################################################################

#' Reads radiosonde data
#' 
#' The sounding data needs to be in the csv format as obtained from University of Wyoming 
#' radiosonde archive. Only one sounding per file is supported.
#' Check: https://weather.arcc.uwyo.edu/upperair/sounding.shtml
#' Select 'Output type': Comma Separated Values
#' 
#' @param url (character) Either the filename or the URL of the sounding csv file.
#' 
#' @return data.frame with sounding data
#' 
#' @author stephan.henne@@empa.ch
#' 
#' @export 
get.sounding = function(url){
  require(stringr)
  
  # helper function to test if 'url' is a web address or file
  is.valid.url <- function(string) {
    pattern <- "(https?|ftp)://[^ /$.?#].[^\\s]*" 
    stringr::str_detect(string, pattern)
  }  

  if (is.valid.url(url)){  
    # name of temporary file for downloading data to
	  tmp.fn = tempfile()	
	  # there are two types of data formats in the database (BUFR, FM35). Default URL is set to BUFR. 
	  # If request fails, type is changed to FM35.
	  rsp = try(download.file(url, tmp.fn, quiet = TRUE))
	  if (class(rsp)=="try-error"){
  	  url = sub("BUFR", "FM35", url)
	    rsp = try(download.file(url, tmp.fn, quiet = TRUE))
	    if (class(rsp)=="try-error"){
	       stop(rsp)
	    }
	  }
	  # read data from temporary file
    dat = read.table(tmp.fn, header=TRUE, sep=",")
    # remove temporary file
    file.remove(tmp.fn)
  } else {
    dat = read.table(url, header=TRUE, sep=",")
  }
  
  # convert relevant names
  names(dat)[grepl("pressure", names(dat))]   = "PRES"
  attr(dat$PRES, "units") = "hPa"
  names(dat)[names(dat)=="temperature_C"]     = "TEMP"
  attr(dat$TEMP, "units") = "°C"
  names(dat)[names(dat)=="mixing.ratio_g.kg"] = "MIXR"  
  attr(dat$MIXR, "units") = "°g kg-1"
  
  return(dat)
}
	
#require(chron)
#dtm=chron("2014-09-01", "12:00:00", format=c("y-m-d", "h:m:s"))
#url = create.sounding.url(dtm, stnm="06610")
#snd = get.sounding(url)



