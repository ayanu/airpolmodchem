#	tt:	ambient temperature	in K
#	sh:	water vapour mixing ratio in kg/kg
#	pp:	ambient pressure in hPa
#
#	returns tv in K
TT_SH_2_TV = function(tt, sh, pp=1013.25){
	msk = which(tt<150)
	if (length(msk)>0) tt[msk] = tt[msk] + 273.15

#	tv = tt * (1. + 0.378 * ES(td) / pp)

	tv = tt * (1. + 0.61 * sh)
	return(tv)
}
