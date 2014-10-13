# TODO: Add comment
# 
# Author: hes
###############################################################################

#	default values are those for Ozone and agricultural land under summer conditions

#	aerodynamic resistance 
#   valid for neutral conditions only
r.a  = function(z, z.0, u.star){
	von.karman = 0.4
	
	r.a = 1/(von.karman*u.star)*log(z/z.0)
	
	return(r.a)
}


#	resistance to molecular diffusion 
r.b = function(u.star=0.5){
	D = 0.000015 	#  molecular diffusivity of O3 in air in m^2/s from Emberson, 2000, Environmental Pollution
	nu = 1.48E-5	#	kinematic viscosity of air at 20°C
	Sc = nu/D		#	Schmidt number
	Pr = 0.713		#	Prandtl number: air at 20°C http://www.engineeringtoolbox.com/air-properties-d_156.html
	z0.m.by.z0.g = 100
	
	r.b = log(z0.m.by.z0.g)*(Sc/Pr)^(2/3)/(0.4 * u.star)		#	equation 20.14	Jacobson
	
	return(r.b)
}

#	GR 		incoming global radiation (W/m2)
#	TT.s	surface temperatuer (°C)
#	R.min	minimal resistance for water vapor (depence on landuse)
#	Dv.by.Dq	ratio of the diffusion coefficient of water vapor to that of gas q

r.stom = function(GR, TT.s, r.min=60, Dv.by.Dq=1.6){

	r.stom = r.min * (1 + (200/(GR + 0.1))^2) * 400/(TT.s*(40-TT.s))*Dv.by.Dq
	
	return(r.stom)
}


#	H.star 	effective Henry's law constant	(mol/l bar-1)
#	f.0 	reactivity factor (unitless; range 0-1)
#	r.cut.0		base cuticle resistance (s m-1) 
r.cut = function(r.cut.0=2000, H.star=1E-2, f.0=1){
	
	r.cut.dry = r.cut.0 * (H.star/1E5 + f.0)
	
	r.cut = r.cut.dry
	
	return(r.cut)
}


#	H.star 	effective Henry's law constant	(mol/l bar-1)
#	f.0 	reactivity factor (unitless; range 0-1)
r.meso = function(H.star=1E-2, f.0=1){
	
	r.meso = 1/(H.star/3000 + 100*f.0)
	
	return(r.meso)
}

#	resistance of gases to buoyant convection 
#	slope 	slope of terrain in radians
r.conv = function(GR, slope=0){
	r.conv = 100 * (1 + 1000/(GR + 10))/(1 + 1000 * slope)
	
	return(r.conv)
}

#	resistance to surfaces other than leaves
r.surf = function(H.star=1E-2, f.0=1, r.surf.O3=1000, r.surf.SO2=2000){
	r.surf = 1/(1E-5*H.star/r.surf.SO2 + f.0/r.surf.O3)
	return(r.surf)
}


##	h.c 	canopy height (m)
##	LAI		one sided leaf area index (m2/m2)
##	u.star	friction velocity (m/s)
#r.canp = function(h.c=14, LAI=2, u.star=0.5){
#	b.c=14	#	constant value 
#	
#	r.canp = b.c*h.c*LAI/u.star
#			
#	return(r.canp)
#}

r.soil = function(H.star=1E-2, f.0=1, r.soil.O3=150, r.soil.SO2=150){
	r.soil = 1/(1E-5*H.star/r.soil.SO2 + f.0/r.soil.O3)
	return(r.soil)
	
}


#' @references Wesely, M. L., 1989: Parameterization of surface resistances to gaseous dry deposition in regional-scale numerical models. Atmospheric Environment (1967), 23, 1293-1304.
#' @return dry deposition velocity in m/s
#' @export 
dry.deposition.velocity = function(u.star, zz, z.0, GR, TT.s, lu=2, season=1, spec="O3", H.star,
		f.0, Dv.by.Dq){
	
	H.star.q = list(SO2=1E5, O3=1E-2, NO2=1E-2, NO=1E-3, HNO3=1E14, H2O2=1E5, HONO=1E5)
	f.0.q = list(SO2=0, O3=1, NO2=.1, NO=0, HNO3=0, H2O2=1, HONO=0.1)
	Dv.by.Dq.q = list(SO2=1.9, O3=1.6, NO2=1.6, NO=1.3, HNO3=1.9, H2O2=1.4, HONO=1.6)
	
	#	load parameters
	data("dry.depo.paras", envir=environment())	
	
	#	land use specific parameters
	
	#	assign parameters according to lu and season
	r.min = dry.depo.para[[season]]$r.min[lu]
	r.cut.0 = dry.depo.para[[season]]$r.cut.0[lu]
	r.soil.O3 = dry.depo.para[[season]]$r.soil.O3[lu]
	r.soil.SO2 = dry.depo.para[[season]]$r.soil.SO2[lu]
	r.surf.O3 = dry.depo.para[[season]]$r.surf.O3[lu]
	r.surf.SO2 = dry.depo.para[[season]]$r.surf.SO2[lu]	
	r.canp = dry.depo.para[[season]]$r.canp[lu]		
	
		
	#	species specific parameters
	if (missing(H.star)){
		if (spec %in% names(H.star.q)){
			H.star = H.star.q[[spec]]
		} else {
			stop("H.star missing without species default.")
		}
	}
	if (missing(f.0)){
		if (spec %in% names(f.0.q)){
			f.0 = f.0.q[[spec]]
		} else {
			stop("f.0 missing without species default.")
		}		
	}	
	if (missing(Dv.by.Dq)){
		if (spec %in% names(Dv.by.Dq.q)){
			Dv.by.Dq = Dv.by.Dq.q[[spec]]
		} else {
			stop("Dv.by.Dq missing without species default.")
		}				
	}
	
	#	aerodynamic resistance
	r.a = r.a(z=zz, z.0=z.0, u.star=u.star)
	
	#	resistance to molecular diffusivity
	r.b = r.b(u.star=u.star)
	
	#	terms in total surface resistance
	r.stom = r.stom(GR=GR, TT.s=TT.s, r.min=r.min, Dv.by.Dq=Dv.by.Dq)
	r.mes = r.meso(H.star=H.star, f.0=f.0)
	r.cut = r.cut(r.cut.0=r.cut.0, H.star=H.star, f.0=f.0)
	
	r.conv = r.conv(GR=GR, slope=0)
	r.surf = r.surf(H.star=H.star, f.0=f.0, r.surf.O3=r.surf.O3, r.surf.SO2=r.surf.SO2)
	
#	r.canp = r.canp(LAI=LAI, u.star=u.star)
	r.soil = r.soil(H.star=H.star, f.0=f.0, r.soil.O3=r.soil.O3, r.soil.SO2=r.soil.SO2)
	
	#	surface resistance
	r.c = 1/(1/(r.stom+r.mes)+1/r.cut+1/(r.conv+r.surf)+1/(r.canp+r.soil))	
	
	return(1/(r.a+r.b+r.c))
}



