"RH_TT_2_TD" <-
function(rh, tt){
    td = ES.inv(ES(tt)*rh/100)
    return(td)
}
