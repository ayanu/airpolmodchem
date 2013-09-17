# zz in m
# returns pp in Pa
isa.zz.2.pp = function(zz=0){
    #   constants
    R0=287.1
    g=9.80665
    pp0 = 101325
    level = c(0, 11000, 20000, 32000, 47000, 51000, 71000)
    gamma = c(-0.0065, 0, 0.001, 0.0028, 0, -0.0028, -0.002)
    tt0 =   c(288.15, 216.65, 216.65, 228.65, 270.65, 270.65, 214.65)
    
    #   determine which layer zz is in 
    idx = as.numeric(cut(zz, breaks=level))
    pp = rep(pp0, length(zz))
    
    for (ii in 1:(length(level)-1)){
        msk = which(idx>=ii)
        if (length(msk)==0) break

        msk = which(idx==ii)        
        if (length(msk)>0){
            if (gamma[ii]==0){
                pp[msk] = pp[msk] * exp(-g/R0/tt0[ii]*(zz[msk]-level[ii]))
            } else {
                pp[msk] = pp[msk] * ((tt0[ii]+gamma[ii]*(zz[msk]-level[ii]))/tt0[ii])^(g/(-gamma[ii]*R0))

            }
        }

        msk = which(idx>ii)        
        if (length(msk)>0){
            if (gamma[ii]==0){
                pp[msk] = pp[msk] * exp(-g/R0/tt0[ii]*(level[ii+1]-level[ii]))                
            } else {
                pp[msk] = pp[msk] * ((tt0[ii]+gamma[ii]*(level[ii+1]-level[ii]))/tt0[ii])^(g/(-gamma[ii]*R0))
            }
        }
    }
    
    return(pp)
}


##   pressure profile 
#zz = seq(0, 50000, 500)
#plot(isa.zz.2.pp(zz)/100, zz/1000, log="x", xlab="Pressure (hPa)", ylab="Altitude (km)")
#axis(1, tck=1, col="gray", lty=2)
#axis(2, tck=1, col="gray", lty=2)
#box()
