# TODO: Add comment
# 
# Author: hes
###############################################################################


###############################################################################
###############################################################################
parcel.cbl.height <- function(snd, d.temp = 0.2, verbose=FALSE, plot=TRUE, unit=c("ASL", "AGL"), 
	zlim=c(0,5000)){
	
	#	TODO  ????	qf quality flag?
	if ("qf" %in% names(snd)) {
		surf = which(snd$qf>62)
		if (length(surf)==0) return(NA)
		surf = min(surf)
	} else surf = 1
	
	
	z0 = snd$zz[surf]
	zlim[1] = z0
	pt0 = snd$pt[surf]+d.temp
	if (verbose) cat("Start altitude:", z0, "Potential Temperature:", snd$pt[surf],"\n")
	
	msk = which(snd$zz>z0 & snd$zz < zlim[2] & !is.na(snd$pt) & !is.na(snd$pp))
	
	if (length(msk)<10) return(NA)
	if (hours(snd$dtm)==0 | hours(snd$dtm)>13) return(NA)
	
	cblmsk = msk[which(snd$pt[msk]<pt0)]
	ftmsk = msk[-which(snd$pt[msk]<pt0)]
	if (length(cblmsk)==0) return(NA)
	
	cbl.top.idx = cblmsk[which.max(snd$zz[cblmsk])]
	ft.bot.idx = ftmsk[which.min(snd$zz[ftmsk])]
	
	zcbl = (pt0-snd$pt[cbl.top.idx])*(snd$zz[ft.bot.idx]-snd$zz[cbl.top.idx])/(snd$pt[ft.bot.idx]-snd$pt[cbl.top.idx]) + snd$zz[cbl.top.idx]
		
	if (plot){
		par(mfcol=c(1,3))
		#   temperature
		plot(snd$pt[msk], snd$zz[msk], ylim=zlim, type="l", xlab=expression(theta~(degree*C)), ylab="Altitude (m) asl", col=2)
		par(new=TRUE)
		plot(snd$sh[msk], snd$zz[msk], ylim=zlim, type="l", xlab="", ylab="", xaxt="n", col=4)
		axis(3)
		mtext("Specific Humidity (g/kg)", 3,3, cex=par("cex"))
		lines(par("usr")[1:2], rep(zcbl,2))
		
		plot(snd$ff[msk], snd$zz[msk], ylim=zlim, type="l", xlab="Wind speed (m/s)", ylab="Altitude (m) asl", col=2)
		par(new=TRUE)
		plot(snd$dd[msk], snd$zz[msk], xlim=c(0,360), ylim=zlim, type="p", xlab="", ylab="", pch=20, col=4, xaxt="n")
		axis(3, at=seq(0,360, 90))
		mtext("Wind Direction (degree)", 3,3, cex=par("cex"))
		
		plot(snd$sh[msk], snd$zz[msk], ylim=zlim, type="l", xlab="Specific Humidity (g/kg)", ylab="Altitude (m) asl")
	}
	
	zcbl = switch(unit[1],
			ASL = zcbl,
			AGL = zcbl - z0)
	
	attr(zcbl, "unit") = unit[1]
	return(zcbl)
}
