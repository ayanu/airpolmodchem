#	sh 	kg/kg water vapor mass/(total air mass)
#	pp 	hPa
SH.PP.2.E = function(sh, pp=1013.25){
	
	e = pp * sh /(0.378*sh + 0.622)
	
	return(e)
}
