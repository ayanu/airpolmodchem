# TODO: Add comment
# 
# Author: hes
###############################################################################

#' Reads radiosonde data
#' 
#' The sounding data needs to be in the ASCII format as obtained from University of Wyoming 
#' radiosonde archive. Only one sounding per file is supported.
#' 
#' @param fn (character) Either the filename or the URL of the sounding ASCII file.
#' 
#' @return data.frame with sounding data
#' 
#' @author stephan.henne@@empa.ch
#' 
#' @export 
get.sounding = function(fn){

	# load the data from a sounding, using the university of wyoming website
	con = file(fn, open="r")
	lines = readLines(con)
	close(con)
		
	
#	print(lines[grepl("Convective Available", lines)])
	
	#	header line 
	hdr = which(grepl("<PRE>", lines))[1]+2
	#	units line
	units = which(grepl("<PRE>", lines))[1]+3
	#	first and last data lines
	frst = which(grepl("<PRE>", lines))[1]+5	
	last = which(grepl("</PRE>", lines))[1]-1
	col.end = regexpr("[A-Z][ \n]", lines[hdr])
	repeat{
		tmp = regexpr("[A-Z][ \n]", substring(lines[hdr], col.end[length(col.end)]+1))
		if (tmp==-1) break		
		col.end = c(col.end, tmp+col.end[length(col.end)])		
	}	
	col.end = c(col.end, nchar(lines[hdr]))
	
	con = file(fn, open="r")
	dat = read.fwf(con, widths=diff(c(0, col.end)), header=FALSE, skip=frst-1, n=last-frst+1)
	close(con)	
	names(dat) = strsplit(lines[hdr], "[ ]+")[[1]][-1]
	
	#	remove reference pressure level 
	if (dat$PRES[1]==1000){
		dat = dat[-1, ]
	}
	
	#	units
	units = strsplit(lines[units], "[ ]+")[[1]][-1]
	for (ii in 1:length(units)){
		attr(dat[[ii]], "units") = units[ii]
	}
	return(dat)
}
	
#require(chron)
#dtm=chron("2014-09-01", "12:00:00", format=c("y-m-d", "h:m:s"))
#url = create.sounding.url(dtm, stnm="06610")
#snd = get.sounding(url)



