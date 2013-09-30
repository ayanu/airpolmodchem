"WS_WD_2_UU_VV" <-
function(ws, wd){
    wd = wd/180.*pi
    uu = -ws*sin(wd)
    vv = -ws*cos(wd)
    return(list(UU=uu, VV=vv))
}
