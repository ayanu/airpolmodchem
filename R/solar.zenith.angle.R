solar.zenith.angle = function(tm=0, lng=0, lat=0){
	return(pi/2. - solar.elevation.angle(tm=tm, lng=lng, lat=lat))
}


solar.hour = function(tm, lng=0){
    b = c(0.000075, 0.001868, -0.032077, -0.014615, -0.040849)

    hour = as.numeric((tm-trunc(tm))*24)
    doy =  as.POSIXlt(tm, "GMT")$yday
    thetan = 2*pi*doy/365
    EQT = b[1] + b[2]*cos(thetan) + b[3]*sin(thetan) + b[4]*cos(2*thetan) + b[5]*sin(2*thetan)

    th = pi*(hour/12-1+lng/180) + EQT

	return(th)
}


solar.elevation.angle <- function(tm=0, lng=0, lat=0){
#    calculation of sun declination angle following Madronich1999a
    a = c(0.006918, -0.399912, 0.070257, -0.006758, 0.000907, -0.002697, 0.001480)
    b = c(0.000075, 0.001868, -0.032077, -0.014615, -0.040849)
    
    hour = as.numeric((tm-trunc(tm))*24)
    doy =  as.POSIXlt(tm, "GMT")$yday
    thetan = 2*pi*doy/365
    
    EQT = b[1] + b[2]*cos(thetan) + b[3]*sin(thetan) + b[4]*cos(2*thetan) + b[5]*sin(2*thetan)

    th = solar.hour(tm=tm, lng=lng)

    delta = a[1] + a[2]*cos(thetan) + a[3]*sin(thetan) + a[4]*cos(2*thetan) + 
			a[5]*sin(2*thetan) + a[6]*cos(3*thetan) + a[7]*sin(3*thetan)
       
    return(asin(sin(delta)*sin(pi*lat/180) + cos(delta)*cos(pi*lat/180)*cos(th)))
}

             
solar.azimuth.angle = function(tm, lng=0, lat=0){
    a = c(0.006918, -0.399912, 0.070257, -0.006758, 0.000907, -0.002697, 0.001480)
    b = c(0.000075, 0.001868, -0.032077, -0.014615, -0.040849)
    
    hour = as.numeric((tm-trunc(tm))*24)
    doy =  as.POSIXlt(tm, "GMT")$yday
    thetan = 2*pi*doy/365
    
    EQT = b[1] + b[2]*cos(thetan) + b[3]*sin(thetan) + b[4]*cos(2*thetan) + b[5]*sin(2*thetan)

    th = solar.hour(tm=tm, lng=lng)

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
