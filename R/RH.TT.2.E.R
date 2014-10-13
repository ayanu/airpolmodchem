#' @export 
"RH.TT.2.E" <-
function (rh, tt){
    e = rh * ES(tt) /100
    return(e)
}
