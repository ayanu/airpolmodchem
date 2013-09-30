# pp in hPa
# returns zz in m
isa.pp.2.zz = function(pp=1013.25){
    #   constants
    R0=R.air
    g=g.0
    zz0 = 0 
    pp0 = c(101325, 22632, 5474.9, 868.02, 110.91, 66.939, 3.9564)
    gamma = c(-0.0065, 0, 0.001, 0.0028, 0, -0.0028, -0.002)
    tt0 =   c(288.15, 216.65, 216.65, 228.65, 270.65, 270.65, 214.65)

	pp = pp * 100 	#	convert to hPa
	
    idx = length(pp0) - as.numeric(cut(pp, breaks=pp0)) 
    zz = rep(zz0, length(pp))
   
    for (ii in 1:(length(pp0)-1)){
        msk = which(idx>=ii)
        if (length(msk)==0) break

        msk = which(idx==ii)        
        if (length(msk)>0){
            if (gamma[ii]==0){
                zz[msk] = zz[msk] - log(pp[msk]/pp0[ii])*R0*tt0[ii]/g
            } else {
                zz[msk] = zz[msk] + (tt0[ii]*(pp[msk]/pp0[ii])^(-gamma[ii]*R0/g) - tt0[ii])/gamma[ii]                
            }
        }

        msk = which(idx>ii)        
        if (length(msk)>0){
            if (gamma[ii]==0){
                zz[msk] = zz[msk] - log(pp0[ii+1]/pp0[ii])*R0*tt0[ii]/g
            } else {
                zz[msk] = zz[msk] + (tt0[ii]*(pp0[ii+1]/pp0[ii])^(-gamma[ii]*R0/g) - tt0[ii])/gamma[ii]                
            }
        }
    }

    return(zz)
}

#   example: demonstrate reconversion
#zz = isa.pp.2.zz(1000)
#print(zz)
#print(isa.zz.2.pp(zz))
