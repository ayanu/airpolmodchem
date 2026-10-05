#' Convert horizontal wind vector components to wind spped and direction
#'
#' Units for wind components 'uu' and 'vv' have to be the same. 
#' Units of returned wind speed will be accordingly. 
#' 'uu' and 'vv' can be vectors or arrays, but have to be of the same shape.
#' 
#' @param uu wind component in west-east direction 
#' @param vv wind component in south-north direction 
#' 
#' @return list of WS (wind speed) and WD (wind direction). Units of WS will be the same as 
#' 			'uu' and 'vv'.
#' 
#' @export 
"UU_VV_2_WS_WD" <-
function(uu, vv){
    wd = (360 + atan2(-uu,-vv)*180/pi) %% 360
    ws = sqrt(uu^2+vv^2)

    return(list(WS=ws, WD=wd))
}
