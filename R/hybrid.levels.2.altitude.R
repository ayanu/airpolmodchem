##########################################################################
#	function: hybrid.levels.2.altitude.agl
##########################################################################
#	parameter:
#   ps  (2d; x,y)	surface pressure (Pa)
#   tt  (3d; x,y,z)	temperature (K)
#	akz (1d; z)	
#	bkz (1d; z)
#

hybrid.levels.2.altitude.agl = function(ps, tt, akz, bkz){

    nn.dim = length(dim(tt))
    nn.lev = length(akz)

	#	3d temperature field
    if (nn.dim==3){
        nn.x = dim(ps)[1]
        nn.y = dim(ps)[2]
        aa.h = array(rep(aa.h, each=prod(dim(ps))), c(nn.x, nn.y, nlev))
        bb.h = array(rep(bb.h, each=prod(dim(ps))), c(nn.x, nn.y, nlev))    

        pp = aa.h + bb.h*array(rep(ps, nlev), c(nn.x, nn.y, nlev))

        zz = array(NA, c(nn.x, nn.y, nlev))

		#	surface 
        zz[,,1] = log(ps/pp[,,1])*287*tt[,,1]/9.81

        for (ii in 2:nlev){
            zz[,,ii] = zz[,,ii-1] + log(pp[,,ii-1]/pp[,,ii])*287*tt[,,ii]/9.81
        }
	#	2d temperature field	(e.g. reduced_gg, second dimension vertial)
    } else if (nn.dim==2){
		nn.xy = dim(tt)[1]
		#	akz with same dimensions as tt
		akz = array(rep(akz, each=nn.xy), c(nn.xy, nn.lev))
		bkz = array(rep(bkz, each=nn.xy), c(nn.xy, nn.lev))
		#	2D pressure field		
        pp = akz + bkz*array(rep(ps,nn.lev), c(nn.xy, nn.lev))
		
        zz = array(NA, c(nn.xy, nn.lev))
        zz[,1] = log(ps/pp[,1])*287*tt[,1]/9.81
        for (ii in 2:nn.lev){
            zz[,ii] = zz[,ii-1] + log(pp[,ii-1]/pp[,ii])*287*tt[,ii]/9.81
        }
    } else{
        warning("dimension of tt not supported. Either 3d or 1d. Returning NULL")
        return(NULL)
    }
    
    return(list(zz=zz, pp=pp))
}

