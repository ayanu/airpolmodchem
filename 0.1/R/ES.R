"ES" <-
function (tt){
    if (any(!is.na(tt)) && any(tt>150, na.rm=TRUE)) tt = tt - T.0
	
    es = 6.112 * exp(17.62*tt / (243.12+tt))    
    return(es)
}
