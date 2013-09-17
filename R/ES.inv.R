"ES.inv" <-
function(es){
    d = log(es/6.112)
    tt = 243.12*d/(17.62-d)
    
    return(tt) 
}

