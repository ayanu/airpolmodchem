#' @export 
"UU_VV_2_WS_WD" <-
function(uu, vv){
    wd = (360 + atan2(-uu,-vv)*180/pi) %% 360
    ws = sqrt(uu^2+vv^2)

    return(list(WS=ws, WD=wd))
}
