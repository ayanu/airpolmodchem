#	tt:	ambient temperature
#	td:	dew point temperature
#	pp:	ambient pressure
TT_TD_2_TV = function(tt, td, pp=1013.25){
	msk = which(tt<150)
	if (length(msk)>0) tt[msk] = tt[msk] + 273.15

	msk = which(td<150)
	if (length(msk)>0) td[msk] = td[msk] + 273.15

	tv = tt * (1. + 0.378 * ES(td) / pp)

	return(tv)
}
