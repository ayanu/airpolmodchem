"global.radiation" <-
function(tm=0, lng=0, lat=0){
    library(chron)

    sinphi = sin(solar.zenith.angle(tm, lng, lat))
    
    globrad = rep(0, length(tm))
    msk = which(sinphi>0)
#   T: transmisivity of the atmosphere
#   Stull 1982
    T = (0.8+0.2*sinphi[msk])
#    T = 1
    globrad[msk] = 1370 * T * sinphi[msk]
    
    return(globrad)
}
