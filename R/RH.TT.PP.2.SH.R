#' @export 
"RH.TT.PP.2.SH" <-
function(rh, tt, pp){
    esat = ES(tt)
    e = rh/100*esat
    r = 0.622*e/(pp-e)	
	sh = r/(r+1)
    return(sh)
}
