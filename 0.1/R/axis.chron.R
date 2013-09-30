"axis.chron" <-
function (side, x, at, format, labels, tz= "GMT", lwd=par("lwd"), lwd.ticks=par("lwd"), ...) 
{
    library(chron)
    mat <- missing(at)
    range <- par("usr")[if (side%%2) 
        1:2
    else 3:4]

    if (!mat) 
        x <- at
    else if (missing(x)) x = range 
    
#   d in seconds
    d <- (range[2] - range[1])*86400

    z <- range(c(range, x[is.finite(x)]))
       
    if (d < 1.1 * 60) {
        sc <- 1
        if (missing(format)) 
            format <- "%S"
    }
    else if (d < 1.1 * 60 * 60) {
        sc <- 60
        if (missing(format)) 
            format <- "%M:%S"
    }
    else if (d < 1.1 * 60 * 60 * 24) {
        sc <- 60 * 24
        if (missing(format)) 
            format <- "%H:%M"
    }
    else if (d < 2 * 60 * 60 * 24) {
        sc <- 60 * 24
        if (missing(format)) 
            format <- "%a %H:%M"
    }
    else if (d < 7 * 60 * 60 * 24) {
        sc <- 60 * 60 * 24
        if (missing(format)) 
            format <- "%a"
    }
    else {
        sc <- 60 * 60 * 24
    }
    # < 50 days
    if (d < 60 * 60 * 24 * 150) {
        zz <- pretty((z*86400)/sc) * sc / 86400
        z = zz
        if (missing(format)) 
            format <- "%b %d"
    }
    # < 1.2 years
    else if (d < 1.2 * 60 * 60 * 24 * 365) {
        class(z) <- c("chron", "dates", "times")
        zz <- as.POSIXlt(z, tz)
        zz$mday[1:2] <- 1
        zz$isdst[1:2] <- zz$hour[1:2] <- zz$min[1:2] <- zz$sec[1:2] <- 0

        zz = seq(zz[1], zz[2], by="month")
        z <- as.chron(zz)
        if (missing(format)) 
            format <- "%b %y"
    } 
    else {
        class(z) <- c("chron", "dates", "times")
        zz <- as.POSIXlt(z, tz)
        zz$mday <- 1
        zz$isdst <- zz$mon <- zz$hour <-  0
        
        zz$year <- pretty(c(min(zz$year), max(zz$year)+0.5))
        msk = which(trunc(zz$year) != zz$year)
        
        if (length(msk)>0){
            zz$mon[msk]=6
            zz$mon[-msk]=0
        }
        zz <- as.POSIXct(zz)
        
        z <- as.chron(zz)
        
        if (missing(format)) {
            if (length(msk)==0) format <- "%Y"
            else format = "%b %y"
        }
        
    }    
    if (!mat) 
        z <- x[is.finite(x)]    

    zz<- zz[z>= range[1] & z <= range[2]]
    
    z <- z[z >= range[1] & z <= range[2]]
            
    class(z) = c("chron", "dates", "times")
    
    if (class(zz)[1] != "POSIXt") zz = as.POSIXlt(z, tz)

    if (missing(labels)){
        labels <- format(zz, format = format)
    }    
    axis(side, at = z, labels = labels, lwd=lwd, lwd.ticks=lwd.ticks, ...)
    invisible(z)
}
