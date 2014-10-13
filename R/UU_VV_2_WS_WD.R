#' @export 
"UU_VV_2_WS_WD" <-
function(uu, vv){
    wd = atan(uu/vv)*180/pi
    msk = which(uu>0 & vv>0)
    wd[msk] = wd[msk]+180
    msk = which(uu>0 & vv<=0)
    wd[msk] = 360+wd[msk]
    msk = which(uu<0 & vv>0)
    wd[msk] = 180+wd[msk]    
    ws = sqrt(uu^2+vv^2)
    return(list(WS=ws, WD=wd))
}
