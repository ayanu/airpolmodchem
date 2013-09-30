
nc.2.vmr = function(nc, TT=20, pp=1013.25){
	#   assume that if temperature smaller 100 is given in °C
	TT[TT<150] = TT[TT<150] + 273.15
	
	#   number of molecules per cm^3
	na = pp*100/(R.star*TT)*N.A/1E6
	
	vmr  = nc/na*1E9	#	ppb 
	
	attr(vmr, "unit") = "ppb"
	return(vmr)
}

