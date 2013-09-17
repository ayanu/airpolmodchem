"TT_PP_2_PT" <-
function (tt, pp){
    if (any(!is.na(tt)) && any(tt<150)) tt = tt+273.15

    kappa = 0.285572
    pt = tt*(1000/pp)^kappa            
    return(pt)
}
