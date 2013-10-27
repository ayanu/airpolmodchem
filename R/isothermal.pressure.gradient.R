isothermal.pressure.gradient = function(z, p0=1013.25, TT=288.15){
	
	#   assume that if temperature smaller 100 is given in °C
	TT[TT<100] = TT[TT<100] + 273.15
	
	p = p0 * exp(-(g.0/R.air/TT*z))
	
	return(p)
}
