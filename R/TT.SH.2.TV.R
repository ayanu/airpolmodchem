#	tt:	ambient temperature	in K
#	sh:	water vapour mixing ratio in kg/kg
#	pp:	ambient pressure in hPa
#
#	returns tv in K
TT.SH.2.TV = function(tt, sh, pp=1013.25){
	if (missing(tt)) stop("need temperature tt")
	if (missing(sh)) stop("need specific humidity sh")
	
	msk = which(tt<150)
	if (length(msk)>0) tt[msk] = tt[msk] + T.0

	tv = tt * (1. + 0.608 * sh)
	return(tv)
}
