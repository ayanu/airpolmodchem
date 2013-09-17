####################################################################################################
#               wind.rose
####################################################################################################
#   purpose:    plot wind rose
#   args:       frec    (2D-matrix) frequency distribution of wind direction and speed
#               fnum    Number of reference circumferences to plot.
#               fint    Frequency steps (in %) between reference circumferences.
#               flab    Parameter indicating which circumferences must be labelled:
#                       0: Label outer circumference only,
#                       1: Label all circumferences,
#                       >1: Label only each flab'th circumference
#                       <0: Do not label any circumference.
#               ang     Angle along which circumferences will be labelled.
#               col     Colors to fill the frequency polygons.
#               mar  Margins vector for the plot (to be passed to 'par').
#               key     Set to FALSE if you do not want a legend of the wind-rose,
#                       that will otherwise be plotted if frequencies are supplied by
#                       speed intervals.
#               uni     Speed units for the legend header.
#               ...     Other graphic parameters passed to plot
#   returns:    invisible()
#   author:     taken from package climatol modified by stephan.henne@empa.ch
#   version:    0.1-051124
#   requires:   -
####################################################################################################
wind.rose <- function (frec, fnum = 4, fint, flab = 1, ang = 3 * pi/16, 
    col = rainbow(nrow(frec), 0.5, 0.92, start = 0.3, end = 0.8), mar = c(0, 0, 1, 0), 
    key = TRUE, key.title = "WS (m/s)", ...) 
{

    tfreq = apply(frec, 2, sum)
    if (missing(fint)){
        maxf = max(tfreq)/sum(tfreq)*100      
        if (is.na(maxf)) maxf=10
        if (maxf>=40) fint=10
        else fint = 5
    }
        
    if (missing(fnum)) fnum = trunc(maxf/fint)+1


    if (is.vector(frec)) 
        nr <- 1
    else nr <- nrow(frec)
    ndir <- length(frec)

    fmax <- fnum * fint
    tot <- sum(frec)
    fr <- 100 * frec/tot
    key <- (nr > 1) && key
    
    if (key) mlf <- 2
    else     mlf <- 1
    
    par(mar = mar)
    fx <- cos(pi/2 - (2 * pi/ndir * 0:(ndir - 1)))
    fy <- sin(pi/2 - (2 * pi/ndir * 0:(ndir - 1)))
    plot(fx, fy, xlim = c(-fmax - mlf * fint, fmax + fint), ylim = c(-fmax - 
        fint, fmax + fint), xaxt = "n", yaxt = "n", xaxs="i", yaxs="i", xlab = "", 
        ylab = "", bty = "n", asp = 1, type = "n", ...)
    
#    if (!missing(arrow.dir) && !is.null(arrow.dir)){
#        seg.dir = (c(arrow.dir-60, arrow.dir+60)) %% 360        
#        if (seg.dir[1]>seg.dir[2]) dirs = seq(seg.dir[1]-360, seg.dir[2])
#        else dirs = seq(seg.dir[1], seg.dir[2])
#
#        xx = c(-(fmax)*sin((dirs+180)/180*pi), 0)
#        yy = c(-(fmax)*cos((dirs+180)/180*pi), 0)
#        polygon(xx, yy, col=0, lwd=3, lty=5) #, density=20, angle=-45)
#
#        seg.dir = (c(arrow.dir-60, arrow.dir+60)+180) %% 360        
#        dirs = seq(seg.dir[1], seg.dir[2])
#        xx = c(-(fmax)*sin((dirs+180)/180*pi), 0)
#        yy = c(-(fmax)*cos((dirs+180)/180*pi), 0)
#        polygon(xx, yy, col=0, lwd=3) #, density=20)
#    }

    if (nr == 1) {
        cx <- fx * fr
        cy <- fy * fr
    } else {
        f <- apply(fr, 2, sum)
        cx <- fx * f
        cy <- fy * f
        for (i in nr:2) {
            f <- f - fr[i, ]
            cx <- c(cx, NA, fx * f)
            cy <- c(cy, NA, fy * f)
        }
    }
    polygon(cx, cy, col = col[nr:1])
    symbols(c(0 * 1:fnum), c(0 * 1:fnum), circles = c(fint * 
        1:fnum), inches = FALSE, add = TRUE)
    segments(0 * 1:ndir, 0 * 1:ndir, fmax * fx, fmax * fy)
    fmaxi <- fmax + fint/4
    text(0, 1.02*fmaxi, "N", font=2)
    text(0, -1.02*fmaxi, "S", font=2)
    text(1.02*fmaxi, 0, "E", font=2)
    text(-1.02*fmaxi, 0, "W", font=2)

    if (flab >= 1) 
        for (i in 1:fnum) if (i %% flab==0) text(i * fint * cos(ang), i * fint * sin(ang), paste(i * fint, "%"))
    else if (flab == 0) 
        text(fmax * cos(ang), fmax * sin(ang), paste(fmax, "%"))
        
    if (key) {
        legend("topleft", inset=0.02, pch=22, pt.bg = col, col=par("fg"), pt.cex=1.5, legend = attr(frec, "row.names"), bg=par("bg"), title=key.title)
    }
    
    invisible()
}


##   EXAMPLE
#require(meteoconv)
#dat = data.frame(UU=rnorm(200), VV=rnorm(200))
#tmp = UU_VV_2_WS_WD(dat$UU, dat$VV)
#dat$WS = tmp$WS
#dat$WD = tmp$WD
#dat$cc = sin(dat$WD/180*pi)+rnorm(200, 1)
#
#res = wind.freq(dat, key.title = "WS (m/s)", fnum=6, fint=2.5, flab=1)
##
##res = wind.rose(dat, ws.para="cc", unit = "CO (ppb)")
