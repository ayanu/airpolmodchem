#' Calculate solar zenith angle
#'
#' The solar zenith angle (or its complementary angle solar elevation 'solar.elevation.angle) 
#' 	defines the Sun's apparent altitude. It is the angle between the sun's rays and the 
#' vertical direction. Minimum value at solar noon. Night-time values larger than pi/2.
#'
#' @param tm date/time object (chron)
#' @param lon Longitude in degrees east
#' @param lat Latitude in degrees north
#'
#' @return Solar zenith angle in radians
#'
#' @export 
#' @export 
solar.zenith.angle = function(tm=0, lon=0, lat=0){
	return(pi/2. - solar.elevation.angle(tm=tm, lon=lon, lat=lat))
}

solar.hour = function(tm, lon=0){
    b = c(0.000075, 0.001868, -0.032077, -0.014615, -0.040849)

    hour = as.numeric((tm-trunc(tm))*24)
    doy =  as.POSIXlt(tm, "GMT")$yday
    thetan = 2*pi*doy/365
    EQT = b[1] + b[2]*cos(thetan) + b[3]*sin(thetan) + b[4]*cos(2*thetan) + b[5]*sin(2*thetan)

    th = pi*(hour/12-1+lon/180) + EQT

	return(th)
}

#' Calculate solar elevation angle
#'
#' The solar elevation angle (or its complementary angle solar zenith 'solar.zenith.angle) 
#' 	defines the Sun's apparent altitude. It is the angle between the sun's rays and local 
#' horizon. Maximum value at solar noon. Night-time values smaller 0.
#'
#' @param tm date/time object (chron)
#' @param lon Longitude in degrees east
#' @param lat Latitude in degrees north
#'
#' @return Solar elevation angle in radians
#'
#' @export 
solar.elevation.angle <- function(tm=0, lon=0, lat=0){
#    calculation of sun declination angle following Madronich1999a
    a = c(0.006918, -0.399912, 0.070257, -0.006758, 0.000907, -0.002697, 0.001480)
    b = c(0.000075, 0.001868, -0.032077, -0.014615, -0.040849)
    
    hour = as.numeric((tm-trunc(tm))*24)
    doy =  as.POSIXlt(tm, "GMT")$yday
    thetan = 2*pi*doy/365
    
    EQT = b[1] + b[2]*cos(thetan) + b[3]*sin(thetan) + b[4]*cos(2*thetan) + b[5]*sin(2*thetan)

    th = solar.hour(tm=tm, lon=lon)

    delta = a[1] + a[2]*cos(thetan) + a[3]*sin(thetan) + a[4]*cos(2*thetan) + 
			a[5]*sin(2*thetan) + a[6]*cos(3*thetan) + a[7]*sin(3*thetan)
       
    return(asin(sin(delta)*sin(pi*lat/180) + cos(delta)*cos(pi*lat/180)*cos(th)))
}

#' Calculate solar azimth angle
#'
#' The solar azimuth angle is the azimuth (horizontal angle with respect to north) of the Sun's 
#' position. This horizontal coordinate defines the Sun's relative direction along the 
#' local horizon, whereas the solar zenith angle (or its complementary angle solar elevation) 
#' 	defines the Sun's apparent altitude (see 'solar.zenith.angle).
#'
#' @param tm date/time object (chron)
#' @param lon Longitude in degrees east
#' @param lat Latitude in degrees north
#'
#' @return Solar azimuth angle in radians
#'
#' @export 
solar.azimuth.angle = function(tm, lon=0, lat=0){
    a = c(0.006918, -0.399912, 0.070257, -0.006758, 0.000907, -0.002697, 0.001480)
    b = c(0.000075, 0.001868, -0.032077, -0.014615, -0.040849)
    
    hour = as.numeric((tm-trunc(tm))*24)
    doy =  as.POSIXlt(tm, "GMT")$yday
    thetan = 2*pi*doy/365
    
    EQT = b[1] + b[2]*cos(thetan) + b[3]*sin(thetan) + b[4]*cos(2*thetan) + b[5]*sin(2*thetan)

    th = solar.hour(tm=tm, lon=lon)

    dec = a[1] + a[2]*cos(thetan) + a[3]*sin(thetan) + a[4]*cos(2*thetan) + 
			a[5]*sin(2*thetan) + a[6]*cos(3*thetan) + a[7]*sin(3*thetan)

	#	solar elevation angle
	se = asin(sin(pi*lat/180)*sin(dec) + cos(pi*lat/180)*cos(dec)*cos(th))
	
	#	solar azimuth
	sa = acos( (sin(dec) - sin(se)*sin(lat/180*pi))/(cos(se)*cos(lat/180*pi)) )
	msk = sin(th)>0
	sa[msk] = 2*pi - sa[msk]
	
	return(sa)
}
