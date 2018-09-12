# TODO: Add comment
# 
# Author: hes
###############################################################################

#	vertical standard deviations depending on distance to source and Pasquill stability class
#	internal function
get.sigma.z = function(x, stability=c("D", "A", "B", "C", "E", "F")){
	stability = match.arg(stability)

	switch(stability,
			# extremely unstable
			"A" = {
				sigma.z = x * 0.20
			},
			# moderately unstable
			"B" = {
				sigma.z = x * 0.12
			},
			# slightly unstable
			"C" = {
				sigma.z = x * 0.08 * 1/sqrt(1+0.0002*x)
			},
			# neutral
			"D" = {
				sigma.z = x * 0.06 * 1/sqrt(1+0.00015*x)
			},
			# "slightly stable"
			"E" = {
				sigma.z = x * 0.03 * 1/(1+0.0003*x)
			},
			# "stable"
			"F" = {
				sigma.z = x * 0.016 * 1/(1+0.0003*x)
			}
	)
	
	return(sigma.z)
}

#	horizontal standard deviations depending on distance to source and Pasquill stability class
#	internal function
get.sigma.y = function(x, stability=c("D", "A", "B", "C", "E", "F")){
	stability = match.arg(stability)

	a = switch(stability,
			"A" = 0.22,
			"B" = 0.16,
			"C" = 0.11,
			"D" = 0.08,
			"E" = 0.06,
			"F" = 0.04
	)
	
	sigma.y = x * a * 1/sqrt(1+0.0001*x)
	
	return(sigma.y)
}


#' Gauss plume calculation
#' 
#' Calculates concentration of a Gaussian plume for a given location (x,y,z), 
#' assuming that the source is located at x=0, y=0 and z=h.e and main wind only in x direction. 
#' Stability classes following the Pasquill definition are used. 
#' 
#' @param x	Location in x direction for which to calculate the Gauss plume concentration, given in m from source. 
#' 			Note that x,y,z can be arrays, but they should be of the same shape.
#' @param y Location in y direction for which to calculate the Gauss plume concentration, given in m from source.
#' @param z Location in z direction for which to calculate the Gauss plume concentration, given in m above ground.
#' @param Q emission flux of the source. Given in kg/s.
#' @param h.e effective emission height. Given in m above ground. 
#' @param u wind speed in main wind direction (x) given in m/s.
#' @param stability Stability class following Pasquill definition. Possible values:
#'				A: extremely unstable
#'				B: moderately unstable
#'				C: slightly unstable
#'				D: neutral
#'				E: slightly stable
#'				F: stable
#' @param h.m Boundary layer height given in m above ground. If missing or NA no boundary layer top is assumed.  
#' 
#' @return Concentration resulting from source strength Q, given in kg/m3
#' 
#' @export 
gauss.plume = function(x, y, z, Q, h.e, u, stability="D", h.m=NULL){
	a = 1   #   a can be used to describe surface deposition; valid values between 1 (no deposition) and 0 (total deposition)
	sigma.z = get.sigma.z(x, stability=stability)
	sigma.y = get.sigma.y(x, stability=stability)
	
	if (!is.null(h.m)){
		if (is.na(h.m) | h.m < 0) h.m = NULL
	}
	
	#   make sure x,y,z have same dimensions

	#	check if reflection at boundary layer top is required
	if (is.null(h.m)){
		#	no boundary layer top given -> no reflection considered
		C = Q/(2*pi*u*sigma.y*sigma.z)* exp(-y^2/(2*sigma.y^2)) * 
			( exp(-(z-h.e)^2/(2*sigma.z^2)) + a * exp(-(z+h.e)^2/(2*sigma.z^2)) )
	} else {
		#	boundary layer top given

		#	check if release height higher than boundary layer top
		if (h.e > h.m){
			#	no concentration anywhere in the boundary layer
			C = array(0, dim=dim(x))
		} else {
			#	consider multiple reflection on boundary layer top and surface
			C = Q/(2*pi*u*sigma.y*sigma.z)* exp(-y^2/(2*sigma.y^2))
			C.z = 0
			
			for (ii in (-3):3){
				
				if (ii == 0){
					h.plus = h.e
					h.minus = h.e
				} else if (ii>0) {
					h.plus = h.e - 2*ii*h.m
					h.minus = -h.e - 2*ii*h.m
				} else {
					h.plus = -h.e - 2*ii*h.m
					h.minus = h.e - 2*ii*h.m
				}
				
				C.m = exp(-(z-h.plus)^2/(2*sigma.z^2)) 
				C.s = a * exp(-(z+h.minus)^2/(2*sigma.z^2))
				C.z = C.z + C.m + C.s
			}
								
			C = C * C.z
			C[z>h.m] = 0
		}
	}
	
	#	make sure no negative or NA values get returned
	C[x<0] = 0
	C[is.na(C)] = 0
	
	return(C)
}


#' Gauss plume calculation (directional)
#' 
#' Calculates concentration of a Gaussian plume for a given location (x,y,z), 
#' assuming that the source is located at x=0, y=0 and z=h.e. Main wind direction can be given.
#' Stability classes following the Pasquill definition are used. 
#' 
#' @param u wind speed in x direction given in m/s.
#' @param v wind speed in x direction given in m/s.
#' @param WS scalar wind speed given in m/s.
#' @param WD wind direction given in degree. 0: wind from north, 90: wind from east, 
#'				180: wind from south, 270: wind from west.
#' @param x	Location in x direction for which to calculate the Gauss plume concentration, 
#'				given in m from source. 
#' 				Note that x,y,z can be arrays, but they should be of the same shape.
#' @param y Location in y direction for which to calculate the Gauss plume concentration, 
#'				given in m from source.
#' @param z Location in z direction for which to calculate the Gauss plume concentration, 
#'				given in m above ground.
#' @param Q emission flux of the source. Given in kg/s.
#' @param h.e effective emission height. Given in m above ground. 
#' @param stability Stability class following Pasquill definition. Possible values:
#'				A: extremely unstable
#'				B: moderately unstable
#'				C: slightly unstable
#'				D: neutral
#'				E: slightly stable
#'				F: stable
#' @param h.m Boundary layer height given in m above ground. If missing or NA no boundary 
#'				layer top is assumed.  
#' 
#' @return Concentration resulting from source strength Q, given in kg/m3
#' 
#' @export 
gauss.plume.2D = function(u=1, v=1, WS, WD, x, y, z, Q, h.e, stability, h.m){
	
	#   turn wind components into wind speed and direction
	if (missing(WS) & missing(WD)){
		tmp = UU_VV_2_WS_WD(u, v)
		WS = tmp$WS
		WD = tmp$WD
	}
	if (is.na(stability)) stability = "D"
	
	#   rotate x, y, z in main wind direction
	alpha = (WD-270)/180*pi
	x.rot = x * cos(alpha) - y * sin(alpha)
	y.rot = x * sin(alpha) + y * cos(alpha)
	z.rot = z
	
	msk = which(x.rot>0)
	#   call gauss.plume
	C = array(0, dim=dim(x))
	C[msk] = gauss.plume(x=x.rot[msk], y=y.rot[msk], z=z.rot[msk], u=WS, Q=Q, h.e=h.e, 
		stability=stability, h.m=h.m)
		
	return(C)
}

#' Gauss plume statistics
#' 
#' Calculates surface concentration of a Gaussian plumes resulting from a point source. This is done 
#' for a given time series of dispersion conditions as defined by wind speed, wind direction
#' stability classes, and boundary layer height. The individual plumes are then averaged to provide
#' the mean concentration during the given period. 
#'  
#' @param xrng Range of grid in x direction, given in m. Needs to be a two element vector.
#' @param yrng Range of grid in y direction, given in m. Needs to be a two element vector.
#' @param dx Resolution of grid in x direction, given in m.
#' @param dy Resolution of gird in y direction, given in m.
#' @param freq a data.frame containing the columns WS for wind speed in m/s, WD for wind direction
#' 				in degree, 'stability' containing the stability class (A-F), 'h.m' the boundary 
#' 				layer height in m above ground, 'freq' the frequency of the conditions. If 'freq' 
#' 				is not contained in the data.frame, it is assumed to be 1 for all conditions.
#' @param Q emission flux of the source. Given in kg/s.
#' @param h.e effective emission height. Given in m above ground. 
#' @param C.ex Threshold concentration given in units ug/m3. If given an additional field counting 
#' 			the number of situations that exceed the given threshold is reported.
#'
#' @return Average concentrations for the area defined by xrng and yrng, given as a list with 
#' 			elements xrng, yrng, and zz (containing the concentrations in units ug/m3). If
#' 			'C.ex' was given and additional field 'nn.ex' will be returned giving the 
#' 			number of threshold exceedances per year.
#' 
#' @export  
average.gauss.plume.from.freq = function(xrng=c(-5000,5000), yrng=c(-5000,5000), dx=50, dy=50, 
		freq, Q, h.e, C.ex=NULL){
	
	x = seq(xrng[1], xrng[2], dx)
	y = seq(yrng[1], yrng[2], dy)
	
	nn = nrow(freq)
	nx = length(x)
	ny = length(y)
	xx = array(rep(x, ny), dim=c(nx,ny))
	yy = array(rep(y, each=nx), dim=c(nx,ny))
	z = array(0, dim=c(nx,ny))
	
	if (is.null(freq$freq)) freq$freq=1
	
	C.s = array(0, dim=c(nx,ny,nn))
	if (!is.null(C.ex)){
		nn.ex = array(0, dim=c(nx,ny))
	}
	C.mean = array(0, dim=c(nx,ny))
	C.max = 0
	for (ii in 1:nn){
	
		C.s[,,ii] = 1E9 * gauss.plume.2D(WS=freq$WS[ii], WD=freq$WD[ii], x=xx, y=yy, z=z, Q=Q, h.e=h.e, stability=freq$stability[ii], h.m=freq$h.m[ii])
		
		#	add to weighted mean 
		C.mean = C.mean + C.s[,,ii]*freq$freq[ii]
		C.max = max(c(C.max, C.s[,,ii]), na.rm=TRUE)
		if (!is.null(C.ex)){
			msk = which(C.s[,,ii]>C.ex)
			if (length(msk)>0){
				nn.ex[msk] = nn.ex[msk] + freq$freq[ii]
			}
		}
#		if (ii %% 100 == 0){
#			filled.contour(C.mean/sum(freq$freq[1:ii]))			
#		}
		
	}
	#	finally calculate weighted mean
	C.mean = C.mean/sum(freq$freq)
	
	C.mean = list(xrng=x, yrng=y, zz=C.mean, C.max=C.max)
	if (!is.null(C.ex)){
		C.mean$nn.ex = nn.ex
	}
	return(invisible(C.mean))
}

#require(AirPolModChem)
#
#x = rep(3000, 11)
#y = rep(0,11)
#z = seq(0,200, 20)
#Q = 7.5E-3 # kg/s
#h.e = 50	# m above ground
#h.m = 60
#u = 1
#
#C  = gauss.plume(x=x, y=y, z=z, Q=Q, h.e=h.e, h.m=h.m, stability="E", u=u)
#plot(C, z)
#print(C[1])

#require(RColorBrewer)
#xrng = c(-5000, 5000)
#yrng = c(-5000, 5000)
#dx = 50
#dy = 50 
#Q = 7.5E-3 # kg/s
#h.e = 50	# m above ground
#C.ex = 200 # ug/m3
#
#data(reh.ts)
#C.s = average.gauss.plume.from.freq(xrng=xrng, yrng=yrng, dx=dy, dy=dy, freq=reh.ts, Q=Q, h.e=h.e, C.ex=C.ex)
#
#png("H:/lectures/Air pollution modelling and chemistry/gauss_model/average.concentration.full.ts.png",
#	height=8, width=12, res=300, units="cm", pointsize=10)
#par(mar=c(4,4,1,1)+.1)
#filled.contour(C.s, xlab="Easting (m)", ylab="Northing (m)", col=brewer.pal(9, "YlOrBr"), 
#		levels=seq(0,18,2), key.title="C (ug/m3)")
#mtext(paste("Max y:", signif(max(C.s$zz, na.rm=TRUE), 4), "(ug/m3)"), 3, -1.5, adj=0.)
#mtext(paste("Max h:", signif(C.s$C.max, 4), "(ug/m3)"), 3, -2.5, adj=0.)
#dev.off()
#
#
#png("H:/lectures/Air pollution modelling and chemistry/gauss_model/number.of.exceedances.full.ts.png",
#		height=8, width=12, res=300, units="cm", pointsize=10)
#par(mar=c(4,4,1,1)+.1)
#filled.contour(x=C.s$x, y=C.s$y, z=C.s$nn.ex, xlab="Easting (m)", ylab="Northing (m)", col=brewer.pal(9, "PuRd"), 
#		levels=seq(0,225, 25), key.title="N")
#dev.off()
#
#
#data(reh.freq)
#C.s = average.gauss.plume.from.freq(xrng=xrng, yrng=yrng, dx=dy, dy=dy, freq=reh.freq, Q=Q, h.e=h.e, C.ex=C.ex)
#
#png("H:/lectures/Air pollution modelling and chemistry/gauss_model/average.concentration.red.ts.png",
#		height=8, width=12, res=300, units="cm", pointsize=10)
#par(mar=c(4,4,1,1)+.1)
#filled.contour(C.s, xlab="Easting (m)", ylab="Northing (m)", col=brewer.pal(9, "YlOrBr"), 
#		levels=seq(0,18,2), key.title="C (ug/m3)")
#mtext(paste("Max y:", signif(max(C.s$zz, na.rm=TRUE), 4), "(ug/m3)"), 3, -1.5, adj=0.)
#mtext(paste("Max h:", signif(C.s$C.max, 4), "(ug/m3)"), 3, -2.5, adj=0.)
#dev.off()
#
#png("H:/lectures/Air pollution modelling and chemistry/gauss_model/number.of.exceedances.red.ts.png",
#		height=8, width=12, res=300, units="cm", pointsize=10)
#par(mar=c(4,4,1,1)+.1)
#filled.contour(x=C.s$x, y=C.s$y, z=C.s$nn.ex, xlab="Easting (m)", ylab="Northing (m)", col=brewer.pal(9, "PuRd"), 
#		levels=seq(0,225, 25), key.title="N")
#dev.off()
