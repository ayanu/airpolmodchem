"WS_WD_angle_2_UU_VV" <-
function(ws, wd, angle){
    wd = wd - angle + 180
    wd = (wd +360) %% 360
    wd = wd/180.*pi
    uu = -ws*sin(wd)
    vv = -ws*cos(wd)
    return(list(U.PER=uu, U.PAR=vv))
}
