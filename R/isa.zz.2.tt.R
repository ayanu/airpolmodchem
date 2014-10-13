# zz in m
# returns tt in K
#' @export 
isa.zz.2.tt = function(zz=0){
    #   constants
    R0=R.air
    g=g.0
    pp0 = 101325
    level = c(0, 11000, 20000, 32000, 47000, 51000, 71000)
    gamma = c(-0.0065, 0, 0.001, 0.0028, 0, -0.0028, -0.002)
    tt0 =   c(288.15, 216.65, 216.65, 228.65, 270.65, 270.65, 214.65)
    
    #   determine which layer zz is in 
    idx = as.numeric(cut(zz, breaks=level))
    tt = rep(tt0[1], length(zz))
    
    for (ii in 1:(length(level)-1)){
        msk = which(idx>=ii)
        if (length(msk)==0) break

        msk = which(idx==ii)        
        if (length(msk)>0){
            if (gamma[ii]==0){
                tt[msk] = tt0[ii]
            } else {
                tt[msk] = tt0[ii]+gamma[ii]*(zz[msk]-level[ii])
            }
        }

    }
    
    return(tt)
}
