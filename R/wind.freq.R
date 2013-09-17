####################################################################################################
#               wind.freq
####################################################################################################
#   purpose: calculate wind statistics and plot wind rose for specified data.frame
#   args:       X   (data.frame) has to contains field "WS" for wind speed and "WD" for win direction in degree
#               ws.breaks   (numeric vector) breaks for different wind speed classes
#               wn.breaks   (numeric vector) breaks for different wind direction classes)
#               col         (numeric vector) color for different wind speed classes (should have one element less than ws.breaks)
#               margen      (4 element numeric vector) works as parameter "mar" in plotting
#               ...         further parameters that are passed to the function rosavent of climatol package
#   returns:    table of wind speed/direction frequencies
#   author:     stephan.henne@empa.ch
#   version:    0.1 050928
#   requires:   climatol
####################################################################################################
wind.freq <- function(X, wd.para="WD", ws.para="WS", wd.breaks=seq(-15, 375, 30), ws.breaks, plot=TRUE, mar=c(0,0,1,0), fnum, fint, key.title, ...){
    
    if (!wd.para %in% colnames(X)){
        cat("Selected wind direction parameter", wd.para, "not in data.frame\n")
        return(NULL)
    }
    if (!ws.para %in% colnames(X)){
        cat("Selected wind speed parameter", ws.para, "not in data.frame\n")
        return(NULL)
    }
    
    if (missing(ws.breaks)){
        ws.breaks = pretty(X[[ws.para]])
    }
    
    if (missing(key.title)) key.title = ws.para
    
    n.ws = length(ws.breaks)-1
    n.wd = length(wd.breaks)-2
    wd.diff = diff(wd.breaks)[1]
    
    freq = matrix(NA, n.ws, n.wd)
    tfreq = vector("numeric", n.wd)
    ws.legend = vector("character", n.ws)
    
    f.ws = cut(X[[ws.para]], ws.breaks)
    f.wd = cut(X[[wd.para]], wd.breaks)
    f.wd[as.numeric(f.wd)==length(levels(f.wd))] = levels(f.wd)[1]
    f.wd = factor(f.wd)
    levels(f.wd) = as.character(wd.breaks[c(-1, -length(wd.breaks))]-wd.diff/2)

    freq = as.data.frame(table(f.ws, f.wd)[,])
    tfreq = table(f.wd)[]

#    for (ii in 1:n.ws){
#        msk = which(X$WS>ws.breaks[ii] & X$WS<=ws.breaks[ii+1])
#        f = hist(X$WD[msk], plot=FALSE, breaks=wd.breaks)$counts
#        f[1] = f[1] + f[length(f)]
#        length(f) = length(f)-1
#        freq[ii, ] = f
#        ws.legend[ii] = paste(ws.breaks[ii], "-", ws.breaks[ii+1])
#    }
#    
#    wd.legend = vector("character", n.wd)
#    for (ii in 1:n.wd) {
#        wd.legend[ii] = paste((wd.breaks[ii]+wd.breaks[ii+1])/2)
#        tfreq[ii] = sum(freq[,ii])
#    }
#    freq = as.data.frame(freq, row.names=ws.legend)
#    colnames(freq)=wd.legend


    if (plot){    
        wind.rose(freq, mar=mar, fnum = fnum, fint=fint, key.title = key.title, ...)
    }
    
    return(freq)
}
