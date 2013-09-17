vmr2nc = function(vf, TT=20, pp=1013.25){
    #   assume that if temperature smaller 100 is given in °C
    TT[TT<100] = TT[TT<100] + 273.15
    #   number of molecules per cm^3
    na = pp*100/(8.3144*TT)*6.0221E23/1E6

    nc = vf*na/1E9  #   1/^cm3
    
    attr(nc, "unit") = "1/cm^3"
    return(nc)
}
