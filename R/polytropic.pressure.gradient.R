# TODO: Add comment
# 
# Author: hes
###############################################################################
#' @export 
polytropic.pressure.gradient = function(z, p0=1013.25, T0=288.15, gamma=g.0/c.p){
	
	#   assume that if temperature smaller 100 is given in °C
	T0[T0<100] = T0[T0<100] + 273.15
	
	p = p0 * ((T0-gamma*z)/T0)^(g.0/R.air/gamma)
	
	return(p)
}


