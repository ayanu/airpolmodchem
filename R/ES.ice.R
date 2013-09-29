"ES.ice" <-
function (tt){
	if (any(!is.na(tt)) && any(tt>150, na.rm=TRUE)) tt = tt - T.0

    es = 6.112 * exp(22.46*tt / (272.62+tt))    
    return(es)
}
