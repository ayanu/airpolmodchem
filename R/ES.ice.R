"ES.ice" <-
function (tt){
    if (any(tt>150, na.rm=TRUE)) tt = tt - 273.15
    es = 6.112 * exp(22.46*tt / (272.62+tt))    
    return(es)
}
