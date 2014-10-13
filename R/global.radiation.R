#' @export 
"global.radiation" <-
function(dtm=0, lon=0, lat=0){
    library(chron)

    sinphi = sin(solar.elevation.angle(dtm, lon, lat))
    
    globrad = rep(0, length(dtm))
    msk = which(sinphi>0)
#   T: transmisivity of the atmosphere
#   Stull 1982
    T = (0.8+0.2*sinphi[msk])

	globrad[msk] = F.0 * T * sinphi[msk]
    
    return(globrad)
}
