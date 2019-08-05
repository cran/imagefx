crop.image <- function(img, xleft, ybottom, xright, ytop, pick) {
    defined <- ls()
    if(missing(xleft)) { xleft=NULL }
    if(missing(xright)){ xright=NULL}
    if(missing(ybottom)){ybottom=NULL}
    if(missing(ytop)){ytop=NULL}
    if(missing(pick)){pick=FALSE}

    if(any(is.null(c(xleft,xright,ybottom,ytop)))) { pick=TRUE }

    ## save the original image in case it get modified later
    img.org <- img

    ## check to see if the image is an array or matrix
    if(length(dim(img))>2) {
        img <- img.org[,,1]
    }

    if(pick==TRUE) {

        keep.picking=TRUE
        while(keep.picking==TRUE) {
            plot.new()
            image2(img.org,main='Pick Lower Left and Top Right Corners')
            pick1 <- locator(2)

             if(length(pick1)==0) {
                 pick1=list()
                 pick1$x=c(1,ncol(img.org)-1)
                 pick1$y=c(1,nrow(img.org)-1)
            }

            rect(pick1$x[1],pick1$y[1],pick1$x[2],pick1$y[2],lwd=3,border='white',lty=3)

            legend('topleft',legend=c('repick'),bg='white',pch=21,col='gray30',pt.bg='red',pt.cex=2)
            legend('topright',legend=c('crop'),bg='white',pch=21,col='gray30',pt.bg='green',pt.cex=2)


            pick2 <- locator(1)
            if(length(pick2)==0) {
                pick2=list(NULL)
                pick2$x=ncol(img.org)
                pick2$y=0
            }

            if(pick2$x>ncol(img.org)/2) { keep.picking=FALSE}
        }

        xleft <- nrow(img) - pick1$y[2]
        xright <- nrow(img) - pick1$y[1]
        ybottom <- pick1$x[1]
        ytop <- pick1$x[2]
    }

    if(length(dim(img.org)) == 2 ) {
        img.crop <- img.org[xleft:xright,ybottom:ytop]
    }

    if(length(dim(img.org)) == 3 ) {

        crop.dim.x <- length(xleft:xright)
        crop.dim.y <- length(ybottom:ytop)
        img.crop=array(dim=c(crop.dim.x,crop.dim.y,3))

        i=1
        while(i<=dim(img.org)[3]) {

            cur.crop <-  img.org[xleft:xright,ybottom:ytop,i]
            img.crop[,,i] <- cur.crop

            i=i+1
        }
    }

    img.crop.return <- list()
    img.crop.return$img.crop <- img.crop
    img.crop.return$img.corners <- c(xleft,xright,ybottom,ytop)

    return(img.crop.return)

}




