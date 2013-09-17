#	sh 	g/kg water vapor mass/(total air mass) 
SH_PP_2_E = function(sh,pp){
	
	e = pp * sh /(0.378*sh + 0.622)
	
	return(e)
}
