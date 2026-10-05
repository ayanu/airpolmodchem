#' Convert horizontal wind speed and direction into vector components
#'
#' Units of returned wind components will be the same as for passed wind speed 'ws'.
#' 'ws' and 'wd' can be vectors or arrays, but have to be of the same shape.
#' 
#' @param ws wind speed
#' @param wd wind direction
#' 
#' @return list of 'uu' (west-east wind component) and 'vv' (south-north wind component). 
#'			Units of WS will be the same as 'ws'.
#' 
#' @export 
"WS_WD_2_UU_VV" <-
function(ws, wd){
    wd = wd/180.*pi
    uu = -ws*sin(wd)
    vv = -ws*cos(wd)
    return(list(UU=uu, VV=vv))
}
