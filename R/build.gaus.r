build.gaus <- function(xdim,ydim,sig.x,sig.y,x.mid,y.mid) {

    if(missing(sig.y)){sig.y=sig.x}
    if(missing(x.mid)) {x.mid <- floor(xdim/2)}
    if(missing(y.mid)) {y.mid <- floor(ydim/2)}
    
    xs = rep(1:xdim,ydim)
    ys = rep(1:ydim,each=xdim)
  
    gaus <- exp(-((xs-x.mid)^2/(2*sig.x^2) + (ys-y.mid)^2/(2*sig.y^2)))
    gaus.mat <- matrix(gaus,nrow=max(xs),ncol=max(ys))/sum(gaus)

    return(gaus.mat)

}


