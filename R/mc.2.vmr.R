
# cc in micro gram / m^3
# pp in hPa
# TT in °C
#' @export 
mc.2.vmr = function(cc, molw, species="CO", TT=20, pp=1013.25){

    if (missing(molw)){
        molw = c(28.0101, 16.0425, 47.998, 46.0055, 30.0061, 44.0128)  #   g per mol
        names(molw) = c("CO", "CH4", "O3", "NO2", "NO", "N2O")
        idx = which(names(molw)==species)
        if (length(idx)==0) {
            warning("Unknown species. No unit conversion done.")
            return(cc)
        }
    } else {
        idx = 1
    }
    
    #   assume that if temperature smaller 100 is given in °C
    TT[TT<100] = TT[TT<100] + 273.15
    
    nc = cc/1E6/molw[idx]	#	moles of target gas per volume
    na = pp*100/(R.star*TT)	#	 moles of air per volume					
    
#    naa = pp*100/(287*(TT+273.15)*28.97)
    vf = nc/na*1E9	#	mole fraction
    
    attr(vf, "unit") = "ppbv"   
    return(vf)
}
