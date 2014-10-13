#' @export 
"RH.TT.2.TD" <-
function(rh, tt){
    td = E.2.TD(ES(tt)*rh/100)
    return(td)
}
