# TODO: Add comment
# 
# Author: hes
###############################################################################


"E.2.TD.ice" <-
		function(e){
	d = log(e/6.112)
	tt = 272.62*d/(22.46-d)
	
	return(tt) 
}
