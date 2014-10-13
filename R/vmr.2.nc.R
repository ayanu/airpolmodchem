#' @export 
vmr.2.nc = function(vmr, TT=20, pp=1013.25){
    #   assume that if temperature smaller 100 is given in °C
    TT[TT<150] = TT[TT<150] + 273.15

	#   number of molecules per cm^3
    na = pp*100/(R.star*TT)*N.A/1E6

    nc = vmr*na/1E9  #   1/cm3
    
    attr(nc, "unit") = "1/cm3"
    return(nc)
}

