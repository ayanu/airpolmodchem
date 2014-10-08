# TODO: Add comment
# 
# Author: hes
###############################################################################


dewpt = function(r,p){
#		"""Returns dewpoint temperature (Celcius) for given mixing ratio (kg/kg)
#		and pressure (hectopascal)"""
	R = 287.04  # gas constant air
	Rv = 461.5  # gas constant vapor
	
	eps = R/Rv
	return(243.5/(17.67/(log((r*p/(r+eps))/6.112))-1))
}


gamma_s = function(T,p){
#		"""Calculates moist adiabatic lapse rate for T (Celsius) and p (Pa)
#		Note: We calculate dT/dp, not dT/dz
#		See formula 3.16 in Rogers&Yau for dT/dz, but this must be combined with 
#		the dry adiabatic lapse rate (gamma = g/cp) and the
#		inverse of the hydrostatic equation (dz/dp = -RT/pg)"""
	# constants used to calculate moist adiabatic lapse rate
	
	# See formula 3.16 in Rogers&Yau
	T_zero = 273.15
	L = 2.5008e6 # latent heat of vaporization
	L = (2.5008E3 - 2.36 * T + 0.0016 * T^2 - 0.00006 * T^3) * 1E3 
	
	Rv = 461.5  # gas constant vapor	
	cp = 1005.
	cv = 718.
	Rs = cp-cv
	eps = Rs/Rv
	kappa = (cp-cv)/cp
	g = 9.80665
	
	a = 2./7. # 2./7.
	b = eps*L^2/(Rs*cp)
	c = a*L/Rs
	esat = ES(T)*100	#	convert to Pa
	wsat = eps*esat/(p-esat) # Rogers&Yau 2.18
	
	numer = a*(T+T_zero) + c*wsat
	denom = p * (1 + b*wsat/((T+T_zero)^2))

	return(numer/denom) # Rogers&Yau 3.16
}


skewnessTerm = function (P,P_bot){
	skewness = 37.5		
	return(skewness * log(P_bot/P))
}


plot.skewT.ax = function(ax=None, xlim=c(-40,40), ylim=c(105000.,10000.), dp=100, main=""){
		
	#	from matplotlib.ticker import FormatStrFormatter,MultipleLocator
	
	# Get values from kwargs
	Tmin = xlim[1]
	Tmax = xlim[2]
	P_b = ylim[1]
	P_t = ylim[2]

	# set some constants
	T_zero = 273.15
	cp = 1005.
	cv = 718.
	kappa = (cp-cv)/cp
	P_top = 10000.
	P_bot = 100000.#P_b # see if this works, was 100000.

	plevs  = seq(P_b,P_t-1,-dp)
	
	
	plot.new()
	plot.window(xlim, ylim, log="y", xaxs="i", yaxs="i")
	axis(1, at=seq(-100,100,10), lwd=par("lwd"))
	
	y.at = seq(1000, 100, -100)*100
	par(las=2)
	axis(2, at=y.at, labels=sprintf("%d", y.at/100), lwd=par("lwd"))
	par(las=1)
	
	# plot isobars
	axis(2, at=y.at, labels=rep("", length(y.at)), tck=1, col="gray", )
	
	box()
	title(ylab="Pressure (hPa)", xlab="Temperature (°C)")
	mtext("Mixing ratio (g/kg)", 1, 2, col="green")
	title(main=main)

	# plot isotherms
	for (temp in seq(-150,50,10)){
		lines(temp + skewnessTerm(plevs,P_b), plevs, col = 'black', lty=1, lwd = par("lwd"))
	}

	# plot dry adiabats
	for (tk in T_zero+seq(-30,210,20)){
		dry_adiabat = tk * (plevs/P_b)^kappa - T_zero + skewnessTerm(plevs,P_b)
		lines(dry_adiabat, plevs, col = 'gold', lty=1, lwd = par("lwd"))
	}
	
	# plot moist adiabats  
	ps = plevs[plevs<=P_bot]
	for (temp in c(seq(Tmin,10,5.),seq(15,Tmax+.1,5))){
		moist_adiabat = numeric()
		for (p in ps){
			temp = temp - dp*gamma_s(temp,p)
			moist_adiabat = c(moist_adiabat, temp  + skewnessTerm(p,P_bot))
		}				
		lines(moist_adiabat, ps, col = 'purple',lty = 2, lwd = par("lwd"))
	}
	
	
# plot mixing ratios
	for (mixr in c(0.1,0.2,0.5,1,2,3,4,6,8,12,16,24,32,48,64)){
		Tdew = dewpt(mixr*0.001,0.01*plevs)
		lines(Tdew + skewnessTerm(plevs,P_bot), plevs, col='green', lty=2, lwd = par("lwd"))
		text(Tdew[1]+ skewnessTerm(plevs[1],P_bot), plevs[1], mixr, col='green', adj=c(0.5,0))
	}

	return(NULL)
}

#' Draw a Skew-T diagram
#' 
#' Draws a Skew-T diagram for a given atmsopheric profile of atmospheric temperature, specific
#' humidity and pressure. Optionally calculates the Lifting Condensation Level and Convective 
#' Available Potential Energy (CAPE).
#' 
#' @param snd (data.frame) should contain at least the columns 'PRES' for atmospheric pressure in 
#' 				hPa, 'TEMP' for atmospheric temperature in °C, and 'MIXR' for the water vapour 
#' 				mixing ratio in g/kg.
#' @param T.surf	optional surface temperature (°C) used for LCL and CAPE calculation
#' @param pv.surf	optional water vapor mixing ratio at the surface (g/kg) used for LCL and CAPE
#' 					calculation
#' @param plot.LCL	(logical) if TRUE lifting condensation level is calculated and displayed
#' @param plot.cape (logical) if TRUE CAPE is calculated and displayed
#' 
#' @return NULL at success
#' 
#' @references Rogers&Yau
#' 
#' @author stephan.henne@@empa.ch
#' 
#' @export
plot.skew.T = function (snd, T.surf, qv.surf, plot.cape=TRUE, plot.LCL=TRUE, ...){
#		'''
#		# This script plots a Skew-T ln(P) diagram. 
#		# 
#		# usage: 
#		#     plot_skew_T(mysounding)  OR   plot_skew_T(mysounding, T_surf=22, qv_surf=12) etc. 
#		# 
#		# input:  
#		#     sounding  (dict) -  should contain the sounding data
#		# 
#		# optional input: 
#		#     T_surf (float)   - the temperature (C) of a parcel at the surface level
#		#     qv_surf (float)  - the specific humidity (g/kg) of a parcel at the surface level
#		# 
#		#     options (string)  - the characters indicate what should be plotted, by default 'l' (LCL) and 'c' (CAPE)
#		'''	
	P = snd$PRES
	T = snd$TEMP
	qv= snd$MIXR
	

	T_zero = 273.15
	P_top = 10000.
	P_bot = 100000.
	cp = 1005.
	cv = 718.
	kappa = (cp-cv)/cp
	g = 9.81
	Rs = cp-cv

	#	
	plot.skewT.ax(...)
	
	if (missing(T.surf)){
		T.surf = T[1]
	}
	
	if (missing(qv.surf)){
		qv.surf = qv[1]
	}
	
	pnew = 100*P
	Td = dewpt(qv*0.001,P)

	Tmin = par("usr")[1]
	Tmax = par("usr")[2]	
	P_b = par("usr")[3]
	P_t = par("usr")[4]
	
	sbi = 1

#################################################### plot parcel trajectories
# surface pressure levels instead of pressure levels

# calculate the dry trajectories for Temp and Td
#mixedlayer_r = sum(rnew[:4])/len(rnew[:4])
#mixedlayer_temp = sum(Tempnew[:4])/len(Tempnew[:4])
#mixedlayer_pres = sum(Presnew[:4])/len(Presnew[:4])


# use mixed layer values
#dryTd = dewpt(mixedlayer_r,0.01*splevs)
#dryT = (T_zero+mixedlayer_temp) * (splevs/mixedlayer_pres)**kappa - T_zero


#+splevs = np.arange(pres,P_t-1,-dp)
#use surface values
#dryTd = dewpt(rv2m,0.01*splevs)

	# calulate trajectory
	dp = 100.
	splevs = seq(pnew[1],pnew[length(pnew)]-1,-dp)
	
	dryTd = dewpt(qv.surf*0.001, splevs*0.01)
	dryT = (T_zero+T.surf) * (splevs/pnew[1])^kappa - T_zero # was: pnew[sbi] 


	
	if (plot.LCL || plot.cape) {
		# calculate the index number of the LCL
		TminTd = abs(dryT-dryTd)
		lcli = which.min(TminTd)
	
		moistT = numeric()
		temp = dryT[lcli]
		for (ps in splevs[lcli:length(splevs)]){
			temp = temp - dp*gamma_s(temp,ps)
			moistT = c(moistT, temp)
		}
	
		# calculate the trajectories arrays
		plotTd = dryTd[1:(lcli-1)]
		plotp = splevs[1:(lcli-1)]
		trajT = c(dryT[1:(lcli-1)], moistT)
	
		# plot LCL
		lcl = splevs[lcli]
	}
	
	#	
	if (plot.LCL) {      
		# plot the trajectories
		lines(plotTd + skewnessTerm(plotp, P_bot), plotp, col = 'purple', lty=1, 
				lwd = 2*par("lwd"))
		lines(trajT  + skewnessTerm(splevs, P_bot), splevs, col = 'purple', lty=1, 
				lwd = 2*par("lwd"))
		
		lines(c(Tmin,Tmax), rep(lcl,2), col = 'purple', lty = 1, lwd = 2*par("lwd"))
		
		text(Tmin, lcl, paste("LCL:", round(lcl/100), "(hPa)"), cex=1.2, adj=c(-0.05, -0.05), 
			font=2, col="purple")
	
		cat("LCL:", lcl, "(Pa)\n")
#		ax.text(Tmin+.5,lcl,'LCL',color='purple',ha='left',va='center',fontsize=14,\
#		bbox=dict(edgecolor='black',facecolor='white', alpha=0.8))    
	}
	################################################ plot temperature and dewpoint temperature


	#	calculate and plot CAPE
	if (plot.cape){
		#calculate cape
		interpT = approx(pnew,T, splevs, rule=3)$y
		Tdiff = Rs/splevs[lcli:length(splevs)] * 
			(trajT[lcli:length(splevs)]-interpT[lcli:length(splevs)])
		
		msk = which(Tdiff>0)
		if (length(msk)>0){
			cape = sum(Tdiff[msk], na.rm=TRUE)*dp
			
			polygon(c(trajT[lcli+msk-1]+skewnessTerm(splevs[lcli+msk-1],P_bot), 
				rev(interpT[lcli+msk-1]+ skewnessTerm(splevs[lcli+msk-1],P_bot))), 
				c(splevs[lcli+msk-1], rev(splevs[lcli+msk-1])), col="#f8eb5088", border=NA)
		} else {
			cape = 0 
		}
		cat("CAPE:", cape, "(J/kg)\n")
		mtext(paste("CAPE:", round(cape), "(J/kg)"), 3, -2, cex=1.2, adj=c(0.05), font=2, col="gold")
	}
	
	
	
	
	# plot the actual graph
	lines(T + skewnessTerm(pnew,P_bot), pnew, col= 'red', lty=1, lwd= 2*par("lwd"))
	lines(Td + skewnessTerm(pnew,P_bot), pnew, col= 'blue', lty=1, lwd= 2*par("lwd"))


	return(invisible(NULL))
}

#require(AirPolModChem)
#dtm=chron("2014-08-01", "12:00:00", format=c("y-m-d", "h:m:s"))
#stnm = "06610"	#	Payerne
#out.dir = "H:\\lectures\\Air pollution modelling and chemistry\\R_generated_plots"
#
#url = create.sounding.url(dtm, stnm=stnm)
#snd = get.sounding(url)
#
#png(file.path(out.dir, paste("Payerne_", chron.2.string(dtm, "%Y%m%d%H"), "_cape.png", sep="")), 
#	height=14, width=12, units="cm", res=600, pointsize=11)
#par(mar=c(4,4,2,1)+.1)
#plot.skew.T(snd, plot.LCL=TRUE, plot.cape=TRUE, main=paste("Payerne", 
#	chron.2.string(dtm, "%Y-%m-%d %H GMT"))) #, T.surf=24, qv.surf=12)
#dev.off()