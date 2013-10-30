
#	dtm 	(chron)		single time 
#	lat		(numeric)	latitude in degrees north
#	lon		(numeric) 	longitude in degrees east
get.MCM.photolysis.rates = function(dtm, lon=0, lat=0) {

	if (length(dtm)>1) stop("dtm has to be of length 1")
	lat = lat / 180 * pi ;
	lon = lon / 180 * pi ;
	
	jday = as.numeric(format(as.POSIXlt(dtm, "GMT"), "%j"))/365. * 2 * pi
	
	#
	eqt = 0.000075 + 0.001868 * cos(jday) - 0.032077 * sin(jday) - 0.014615 * cos(2*jday) - 
		0.040848 * sin(2*jday)
	#	declination
	dec = 0.006918 - 0.399912 * cos(jday) + 0.070257 * sin(jday) - 0.006758 * cos(2*jday) + 
		0.000907 * sin(2*jday) - 0.002697 * cos(3*jday) + 0.001480 * sin(3*jday) 

	costime = cos(2*pi*as.numeric(dtm) + lon + eqt)	
	#	cosine of solar zenith angle 
	cosx = sin(lat) * sin(dec) - cos(lat) * cos(dec) * costime
	secx = 1/cosx
	
	#	parameterisation taken from MCM3.1
	J = numeric(61)

	J[1] = 6.073E-05*(cosx^(1.743))*exp(-0.474*secx) 
	J[2] = 4.775E-04*(cosx^(0.298))*exp(-0.080*secx) 
	J[3] = 1.041E-05*(cosx^(0.723))*exp(-0.279*secx) 
	J[4] = 1.165E-02*(cosx^(0.244))*exp(-0.267*secx) 
	J[5] = 2.485E-02*(cosx^(0.168))*exp(-0.108*secx) 
	J[6] = 1.747E-01*(cosx^(0.155))*exp(-0.125*secx) 
	J[7] = 2.644E-03*(cosx^(0.261))*exp(-0.288*secx) 
	J[8] = 9.312E-07*(cosx^(1.230))*exp(-0.307*secx) 
	J[11] = 4.642E-05*(cosx^(0.762))*exp(-0.353*secx) 
	J[12] = 6.853E-05*(cosx^(0.477))*exp(-0.323*secx) 
	J[13] = 7.344E-06*(cosx^(1.202))*exp(-0.417*secx) 
	J[14] = 2.879E-05*(cosx^(1.067))*exp(-0.358*secx) 
	J[15] = 2.792E-05*(cosx^(0.805))*exp(-0.338*secx) 
	J[16] = 1.675E-05*(cosx^(0.805))*exp(-0.338*secx) 
	J[17] = 7.914E-05*(cosx^(0.764))*exp(-0.364*secx) 
	J[18] = 1.140E-05*(cosx^(0.396))*exp(-0.298*secx) 
	J[19] = 1.140E-05*(cosx^(0.396))*exp(-0.298*secx) 
	J[21] = 7.992E-07*(cosx^(1.578))*exp(-0.271*secx) 
	J[22] = 5.804E-06*(cosx^(1.092))*exp(-0.377*secx) 
	J[23] = 1.836E-05*(cosx^(0.395))*exp(-0.296*secx) 
	J[24] = 1.836E-05*(cosx^(0.395))*exp(-0.296*secx) 
	J[31] = 6.845E-05*(cosx^(0.130))*exp(-0.201*secx) 
	J[32] = 1.032E-05*(cosx^(0.130))*exp(-0.201*secx) 
	J[33] = 3.802E-05*(cosx^(0.644))*exp(-0.312*secx) 
	J[34] = 1.537E-04*(cosx^(0.170))*exp(-0.208*secx) 
	J[35] = 3.326E-04*(cosx^(0.148))*exp(-0.215*secx) 
	J[41] = 7.649E-06*(cosx^(0.682))*exp(-0.279*secx) 
	J[51] = 1.588E-06*(cosx^(1.154))*exp(-0.318*secx) 
	J[52] = 1.907E-06*(cosx^(1.244))*exp(-0.335*secx) 
	J[53] = 2.485E-06*(cosx^(1.196))*exp(-0.328*secx) 
	J[54] = 4.095E-06*(cosx^(1.111))*exp(-0.316*secx) 
	J[55] = 1.135E-05*(cosx^(0.974))*exp(-0.309*secx) 
	J[56] = 7.549E-06*(cosx^(1.015))*exp(-0.324*secx) 
	J[57] = 3.363E-06*(cosx^(1.296))*exp(-0.322*secx) 
	J[61] = 7.537E-04*(cosx^(0.499))*exp(-0.266*secx) 

	J[!is.finite(J)] = 1E-30
	
	return(J)
}

get.KPP.species = function(){
	
	nn.spec = .Fortran("R_GetNumSpec", numspec=integer(1))$numspec

	spec.names = character(nn.spec)

	for (ii in 1:nn.spec){
		spec.names[ii] = .Fortran("R_GetSpeciesName", spec=character(1), idx=as.integer(ii))$spec
	}
	spec.names = gsub("[ ]", "", spec.names)

	return(spec.names)
}

get.KPP.globals = function(){
	tmp = .Fortran("R_GetNumGlobals", R_nusrgl=integer(1), R_nusrnames=integer(1))
	nn.usrgl = tmp$R_nusrgl
	nn.usrnames = tmp$R_nusrnames

	global.names = character(nn.usrnames)
	global.length = integer(nn.usrnames)

	for (ii in 1:nn.usrnames){
		tmp = .Fortran("R_GetGlobalsName", R_usrgl_name=character(1), R_usrgl_len=integer(1),
			idx=as.integer(ii))
		global.names[ii] = tmp$R_usrgl_name
		global.length[ii] = tmp$R_usrgl_len
	}
	global.names = gsub("[ ]", "", global.names)
	start = cumsum(c(1, global.length)[1:nn.usrnames])

	return(data.frame(name=global.names, length=global.length, start=start, stringsAsFactors=FALSE))
}

set.KPP.globals = function(vals.gl, tt){
	globals = get.KPP.globals()
	nn.gl.names = nrow(globals)
	tmp = .Fortran("R_GetNumGlobals", R_nusrgl=integer(1), R_nusrnames=integer(1))
	nn.gl = tmp$R_nusrgl
	
	gl.vals = rep(as.numeric(NA), nn.gl)
	for (ii in 1:length(vals.gl)){
		idx = which(globals$name == names(vals.gl)[ii])
		if (length(idx)!=1) next
		idx = globals$start[idx]:(globals$start[idx]+globals$length[idx]-1)
		if (any(idx)>nn.gl) next
		gl.vals[idx] = vals.gl[[ii]]
	}

	msk = which(!is.na(gl.vals))
	if (length(msk)>0){
		tmp = .Fortran("R_SetSelectedGlobals", as.integer(length(msk)), as.integer(msk), 
			as.double(gl.vals[msk]))
	}

	if(!missing(tt)){
		tmp = .Fortran("R_SetTEMP", as.double(tt))
	}
}


#	init.var	(list) initial values of all species
#	var.default (numeric) inital value for species not in init.var
#	dt			(numeric)	time step in seconds
#	tt 			(numeric) 	temperature in K
#
#	init.gl		(list) initial values of user defined globals
init.KPP = function(init.var=NULL, var.default=0, dt, tt=270, init.gl = NULL){

	#	species
	spec = get.KPP.species()
	nn.spec = length(spec)
	vals = rep(as.numeric(var.default), nn.spec)

	if (!is.null(init.var)){
		for (ii in 1:length(init.var)){
			idx = which(spec==names(init.var)[ii]) 
			if (length(idx)==0) next
			if (idx>nn.spec) next
			vals[idx] = init.var[[ii]]
		} 
	}
	.Fortran("R_Initialize", as.double(vals), as.double(dt), as.double(tt))

	if (!is.null(init.gl)){
		# 	user defined globals
		globals = get.KPP.globals()
		nn.gl.names = nrow(globals)
		tmp = .Fortran("R_GetNumGlobals", R_nusrgl=integer(1), R_nusrnames=integer(1))
		nn.gl = tmp$R_nusrgl
		
		gl.vals = rep(as.numeric(0), nn.gl)
		for (ii in 1:length(init.gl)){
			idx = which(globals$name == names(init.gl)[ii])
			if (length(idx)!=1) next
			idx = globals$start[idx]:(globals$start[idx]+globals$length[idx]-1)
			if (any(idx)>nn.gl) next
			gl.vals[idx] = init.gl[[ii]]
		}
		tmp = .Fortran("R_SetAllGlobals", as.double(gl.vals))
	}

	return(vals)
}

update.deposition = function(spec, conc, vd=NULL, dt=1, dz=100){
	nn.spec = length(spec)

	dconc = rep(0., nn.spec)
	if (!is.null(vd)){
		for (ii in 1:length(vd)){
			idx = which(spec==names(vd)[ii])
			if (length(idx)==1){
				dconc[idx] = conc[idx] * (exp(- vd[[ii]] / dz * dt) - 1)
				if((conc[idx]+dconc[idx])<0) dconc[idx] = -conc[idx]
			}
		}
	}
	return(dconc)
}

#	E (list) emissions by name, units: kg/s
#	mu (list) molar mass by name, units: g/mole
#	dt	(numeric) time step, units: s
#	V	(numeric)	volume of box, units: m3 
update.emissions = function(spec, E=NULL, mu=NULL, dt=1, V=1, dtm=chron(0), hour.profile=NULL){
	nn.spec = length(spec)

	dconc = rep(0., nn.spec)
	if (!is.null(E)){
		for (ii in names(E)){
			idx = which(spec==ii)
			if (length(idx)==1){
				dconc[idx] = E[[ii]]*1E3/mu[[ii]] * N.A /(V*1E6) * dt
			}
		}
	}

	if (!is.null(hour.profile)){
		hr = hours(dtm) + 1
		dconc = dconc * hour.profile[hr]
	}

	return(dconc)
}

update.KPP.concentrations = function(conc, dconc){
	nn.spec = length(conc)
	
	if (!missing(dconc)){
		tmp = conc+dconc
		tmp[tmp<0.] = 0.
		
	} else {
		tmp = conc
	}
	
	.Fortran("R_SetAllConc", as.double(tmp))
	
	return(tmp)
}



