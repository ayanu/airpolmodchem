# TODO: Add comment
# 
# Author: hes
###############################################################################


bulk.richardson.cbl.height = function(snd, ri.cr=0.25, plot=TRUE, unit=c("ASL", "AGL"), zlim=c(0,5000), smooth=FALSE){
	
	#   make sure wind components are available
	if (!"uu" %in% names(snd) || !"vv" %in% names(snd)){
		tmp = WS_WD_2_UU_VV(snd$ff, snd$dd)
		snd$uu = tmp$UU
		snd$vv = tmp$VV
	}

	#	5 point gliding average
	if (smooth){
		require(kza)
		snd$pt = kz(snd$pt, 5, 1)
		snd$sh = kz(snd$sh, 5, 1)
		snd$uu = kz(snd$uu, 5, 2)
		snd$vv = kz(snd$vv, 5, 2)
	}
	if ("qf" %in% names(snd)) {
		surf = which(snd$qf>62)
		if (length(surf)==0) return(NA)
		surf = min(surf)
	} else surf = 1
	
	
	#	copy surface values
	z0 = snd$zz[surf]
	zlim[1] = z0
	pt0 = snd$pt[surf]
	uu.0 = snd$uu[surf]
	vv.0 = snd$vv[surf]
	msk = which(snd$zz>z0 & snd$zz < zlim[2] & !is.na(snd$pt) & !is.na(snd$pp))
	
	#	TODO: msk never used in the following
	
	#   calculate bluk richardson number (see Stull p 177)
	nn = length(snd$pt)
	delta.pt = snd$pt[-1] - snd$pt[-nn]
	delta.zz = snd$zz[-1] - snd$zz[-nn]
	delta.U = snd$uu[-1] - snd$uu[-nn]
	delta.V = snd$vv[-1] - snd$vv[-nn]
	
	
	#   local bulk Richardson Number
	rb = 9.8065/snd$pt[-nn]*(delta.pt)*(delta.zz)/(delta.U^2 + delta.V^2)
	#   integrated bulk Richardson Number
	rb.2 = 9.8065/pt0 * (snd$pt - pt0) * (snd$zz - z0) / ((snd$uu-uu.0)^2 + (snd$vv-vv.0)^2)
	
	#	first level where Richardson number below threshold
	idx.cbl = which(rb.2>ri.cr)[1]
	zcbl = snd$zz[idx.cbl]
	
	
	if (plot){
		par(mfcol=c(1,3), cex=1.2, mar=c(4,4,4,1)+.1, lwd=2)
		#   temperature
		plot(snd$pt[msk], snd$zz[msk], ylim=zlim, type="l", xlab=expression(theta~(degree*C)), ylab="Altitude (m) asl", col=2)
		par(new=TRUE)
		plot(snd$O3[msk], snd$zz[msk], ylim=zlim, type="l", xlab="", ylab="", xaxt="n", col=4)
		axis(3)
		mtext("Ozone (ppb)", 3,3, cex=par("cex"), col=4)
		lines(par("usr")[1:2], rep(zcbl,2), col=3)
		
#        lines(par("usr")[1:2], rep(zcbl,2))
		
		plot(snd$ff[msk], snd$zz[msk], ylim=zlim, type="l", xlab="Wind speed (m/s)", ylab="Altitude (m) asl", col=2)
		par(new=TRUE)
		plot(snd$dd[msk], snd$zz[msk], xlim=c(0,360), ylim=zlim, type="p", xlab="", ylab="", pch=20, col=4, xaxt="n")
		axis(3, at=seq(0,360, 90))
		mtext("Wind Direction (degree)", 3,3, cex=par("cex"), col=4)
		lines(par("usr")[1:2], rep(zcbl,2), col=3)
		
		plot(rb.2[msk], snd$zz[msk], ylim=zlim, type="l", xlab="Bulk Richardson Number", ylab="Altitude (m) asl", xlim=c(-10,10), col=2)
		lines(rb[msk], snd$zz[msk], col=4)
		lines(rep(ri.cr, 2), par("usr")[3:4], col="gray")
		lines(par("usr")[1:2], rep(zcbl,2), col=3)
		mtext(paste("Rc =", ri.cr), 3, 1, col="gray", cex=1.5)
	}
	
	
	
}
