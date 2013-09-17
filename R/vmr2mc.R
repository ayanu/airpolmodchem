vmr2mc = function(vf, molw, species="CO", TT=20, pp=1013.25){
    if (missing(molw)){
        molw = c(28.0101, 16.0425, 47.998, 46.0055, 30.0061, 44.0128)  #   g per mol
        names(molw) = c("CO", "CH4", "O3", "NO2", "NO", "N2O")
        idx = which(names(molw)==species)
        if (length(idx)==0) {
            warning("Unknown species")
            return(NA)
        }
    } else {
        idx = 1
    }

    #   assume that if temperature smaller 100 is given in °C
    TT[TT<100] = TT[TT<100] + 273.15
    #   number of moles per m^3
    na = pp*100/(8.3144*TT)

    mc = vf/1E9*na*molw[idx]*1E6
        
    attr(mc, "unit") = "ug/m^3"
    return(mc)
}
