"TT_TD_2_RH" <-
function (tt, td){
    rh = ES(td)/ES(tt)*100
    return(rh)
}
