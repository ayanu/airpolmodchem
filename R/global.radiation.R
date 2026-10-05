#' Calculate clear sky global radiation at time and location 
#' 
#' Using simplified equation based on soloar elevation angle as given in Stull 1982 
#' 
#' @param dtm date/time as chron object
#' @param lon longitude in degree east 
#' @param lat latitude in degree north
#' 
#' @return global radidation in units W m-2
#' 
#' @export 
#' @import chron
"global.radiation" <-
function(dtm=0, lon=0, lat=0){

    sinphi = sin(solar.elevation.angle(dtm, lon, lat))
    
    globrad = rep(0, length(dtm))
    msk = which(sinphi>0)
#   T: transmisivity of the atmosphere
#   Stull 1982
    T = (0.8+0.2*sinphi[msk])

	globrad[msk] = F.0 * T * sinphi[msk]
    
    return(globrad)
}
