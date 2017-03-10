#' @export 
ts.plot = function(X, para=names(X)[2], xlim, ylim=NULL, xlab, ylab=para, 
	pch, lty, col, leg.pos="topleft", stacked=0, accumulated=FALSE, legend, 
	dtm.col="dtm", flag=NULL, bg.flag=NULL, col.par, color.palette = blue.red.colors, 
	scale = NULL,
	...){

    nn.col = 40
    if(!dtm.col %in% names(X)) stop(paste("Date time field '", dtm.col, "' missing.", sep=""))
    if (class(X[[dtm.col]])[1]!="chron") {
        dtm.is.chron = FALSE
    } else{
        dtm.is.chron = TRUE
    }
    nn.para = length(para)
	
	if (!is.null(scale)){
		for (ii in 1:nn.para){
        	X[[para[ii]]] = X[[para[ii]]] * scale
    	}
	}
        
    if (missing(xlim)) xlim = range(X[[dtm.col]], na.rm=TRUE)
    if (missing(xlab)){
        if (dtm.is.chron){
            xlab = paste(xlim, collapse=" - ")
        } else {
            xlab = "Time"
        }
    }

    if (accumulated && stacked>0){
        stacked = 0
        warning("No stacked plot possible in accumulated mode")
    }
	if (stacked==0){
		ylab = ylab[1]
	}

	if (stacked>0){
		para = rev(para)
		if (!missing(pch)) pch = rev(pch)
		if (!missing(lty)) lty = rev(lty)
		if (!missing(ylab)) ylab = rev(ylab)
		if(missing(col.par) && !missing(col)){
			col = rev(col)
		}	
	}

    if (!missing(col.par)){
        if (length(col.par)<length(para)){
            col.par = rep(col.par, length.out=length(para))
        }
    }    

#   standard colors, line types, etc. 
#    if (missing(pch)) pch = 1:nn.para
#    if (length(pch)<nn.para) pch = rep(pch, nn.para)[1:nn.para]
#    if (missing(lty)) lty = 1:nn.para
#    if (length(lty)<nn.para) lty = rep(lty, nn.para)[1:nn.para]    
#    if (missing(col)) col = 1:nn.para #rainbow(nn.para)
#    if (length(col)<nn.para) col = rep(col, nn.para)[1:nn.para]    
    max.col = 5
    max.pch = 5
    std.col = c(2,3,4,1,5)
    if (missing(col)){ 
        col = rep(std.col[1:max.col], each=max.pch, length=nn.para)
    }
    if (length(col)<nn.para) col = rep(col, nn.para)[1:nn.para]        
    col = as.list(col)    

    if (missing(pch)) {
        pch = rep(21:25, length=nn.para)
    }
    if (length(pch)<nn.para) pch = rep(pch, nn.para)[1:nn.para]

    if (missing(lty)) lty = rep(1, nn.para)
    if (length(lty)<nn.para) lty = rep(lty, nn.para)[1:nn.para]    


    if (missing(legend)) legend = para 
    
    if (accumulated){
        if (nn.para>1){
            for (ii in 2:nn.para){
                X[[para[ii]]] = X[[para[ii]]] + X[[para[ii-1]]]
            }
        }
    }

    msk = which(X[[dtm.col]] >= xlim[1] & X[[dtm.col]] <= xlim[2])
    if (is.null(ylim)) {
        if (stacked == 0){            
            for (ii in 1:nn.para){
                ylim = range(c(ylim, X[[para[ii]]][msk]), na.rm=TRUE)
            }
            if (accumulated){
                ylim[1] = 0
            }            
        }
    }
	if (is.list(ylim)) {
		if (stacked>0){
			ylim.all = rev(ylim)
		} else {
			ylim.all = ylim
		}
	}

    if (stacked > 0){
        org.par = par("mar")
        layout(matrix((nn.para/stacked+2):1, ncol=1), heights=c(0.1, rep(1,nn.para/stacked), 0.3))
        par(mar=c(0,4.1,0,4.1))
        plot.new()
    }
    
	#	loop over parameters to plot
    for (ii in 1:nn.para){
        
        #   use flagging information to exclued invalid points and to indicate non-background flags
        if (paste(para[ii], "flag", sep="") %in% names(X)){
            if (!is.null(flag)) fmsk = which(X[[paste(para[ii], "flag", sep="")]][msk] %in% flag | is.na(X[[paste(para[ii], "flag", sep="")]][msk]))
            else fmsk = 1:length(msk)
            if (!is.null(bg.flag)) bmsk = which(X[[paste(para[ii], "flag", sep="")]][msk] %in% bg.flag | is.na(X[[paste(para[ii], "flag", sep="")]][msk]))
            else bmsk = numeric()
        } else {
            fmsk = 1:length(msk)
            bmsk = numeric()
        }

        if (!missing(col.par)){
            if(col.par[ii] %in% names(X)){
				if (!is.null(color.palette)){
                	col.rng = range(X[[col.par[ii]]][c(fmsk, bmsk)], na.rm=TRUE)
	                tmp.col = round((X[[col.par[ii]]]-col.rng[1])/diff(col.rng)*(nn.col+1)+1)
    	            tmp.col[tmp.col<1] = 1
        	        tmp.col[tmp.col>nn.col] = nn.col
	                col[[ii]] = color.palette(nn.col)[tmp.col]                
				} else {
					if (ii ==1){
						org.col = unlist(col)
					}
					col[[ii]] = org.col[X[[col.par[ii]]]]
				}
            }            
        }
 

        if (stacked > 0){
            if (is.null(ylim)){
                ylim = range(X[[para[ii]]][msk][c(fmsk, bmsk)], na.rm=TRUE)
                if (!all(is.finite(ylim))) ylim=c(0,1)
                else ylim = ylim - c(1,-1)*diff(ylim)*0.04
            } else {
				ylim = ylim.all[[ceiling(ii/stacked)]]
			}
        }

        if (stacked > 0 || ii==1){
            if (stacked > 0 ) {
				if ((ii-1) %% stacked == 0){
					plot.new()
	                plot.window(xlim, ylim, yaxs="i")
				}
            } else{
            	plot.new()
				if (accumulated){
	                plot.window(xlim, ylim, yaxs="i", xaxs="i")
				} else {
	                plot.window(xlim, ylim)
				}
            }
            if (stacked == 0) {
                box()
            } else {
                segments(par("usr")[1], par("usr")[3], par("usr")[1], par("usr")[4])
                segments(par("usr")[2], par("usr")[3], par("usr")[2], par("usr")[4])                
                if (ii==1){
                    segments(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[3])                
                }
                if (ii==nn.para){
                    segments(par("usr")[1], par("usr")[4], par("usr")[2], par("usr")[4])                
                }                
            }
            if (ii==1) {
                if (dtm.is.chron) {
                    axis.chron(1, lwd=par("lwd"))
                } else {
                    axis(1, lwd=par("lwd"))
                }
                mtext(xlab, 1, 3, cex=par("cex"))
            }
			if (stacked>0){
				axis(ifelse(ceiling(ii/stacked)%%2==0,4,2), lwd=par("lwd"))	
				mtext(ylab[ii], ifelse(ceiling(ii/stacked)%%2==0,4,2),3, 
					cex=par("cex"))	
			} else {
	            axis(2, lwd=par("lwd"))
    	        mtext(ylab[ii], 2, 3, cex=par("cex"))            
			}
        } else {
#            title(xlab=xlab, ylab=ylab)
        }
        if (accumulated){
            if (ii == 1){
                polygon(c(X[[dtm.col]][msk], rev(X[[dtm.col]][msk])), c(X[[para[ii]]][msk], rep(0, length(msk))), col=col[[ii]], border=NA)            
            } else{
                polygon(c(X[[dtm.col]][msk], rev(X[[dtm.col]][msk])), c(X[[para[ii]]][msk], rev(X[[para[ii-1]]][msk])), col=col[[ii]], border=NA)
            }
        }else{
			if (is.null(X[[para[ii]]])) {
				if(stacked > 0) {
					ylim = NULL
					text(mean(par("usr")[1:2]), mean(par("usr")[3:4]), "data missing")
				}
				next
			}
            if (!is.na(lty[ii])){            
                if (length(fmsk)>0) lines(X[[dtm.col]][msk][fmsk], X[[para[ii]]][msk][fmsk], lty=lty[ii], col=col[[ii]], ...)
            }

            if (!is.na(pch[ii])){
                if (length(fmsk)>0) points(X[[dtm.col]][msk][fmsk], X[[para[ii]]][msk][fmsk], pch=pch[ii], col=col[[ii]], bg=col[[ii]], ...)
                if (length(bmsk)>0) points(X[[dtm.col]][msk][bmsk], X[[para[ii]]][msk][bmsk], pch=pch[ii], col=col[[ii]], ...)
            }
        }
        
        if (stacked > 0 && !exists("ylim.all")){
            ylim = NULL
        }
    }
 
    if (stacked == 0 && !is.null(legend)){
        if (nn.para>1){
            if (accumulated){
                legend(x=leg.pos, pch=15, col=sapply(col, "[[", 1), legend=legend, bg=par("bg"))
            } else {
                legend(x=leg.pos, pch=pch, col=sapply(col, "[[", 1), lty=lty, legend=legend, bg=par("bg"))
            }
        }
    }

}
