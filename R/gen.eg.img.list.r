gen.eg.img.list <- function(dim.x, dim.y, fps, n.secs, sig.f, sig.peak, box.width, box.height) {

########################
### MISSING ANYTHING ###
########################

    if(missing(dim.x)) { dim.x = 64 }
    if(missing(dim.y)) { dim.y = 64 }
    if(missing(fps)) {fps = 30}
    if(missing(n.secs)) { n.secs = 3 }
    if(missing(sig.f)) { sig.f = 2 }
    if(missing(sig.peak)) { sig.peak = 0.2 }
    if(missing(box.width)) { box.width = 10 }
    if(missing(box.height)) { box.height = 10 }


    ## generate a time axis
    tax <- (1:(n.secs*fps)) / fps 

    ## define a time varying signal
    sig <- sin(2*pi*sig.f*tax)

    ## generate the x and y vaues
    img.xs <- 1:dim.x
    img.ys <- 1:dim.y

    ## middle of image
    mid.x <- dim.x/2
    mid.y <- dim.y/2

    ## define the box bounds
    box.xs <- (mid.x-box.width/2):(mid.x+box.width/2)
    box.ys <- (mid.y-box.height/2):(mid.y+box.height/2)

    ## initilize an image list
    img.list = list()

    ## generate a series of time varying images defined by signal
    i=1
    while(i<=length(tax)) {

        ## generate some noise
        noise <- matrix(runif((dim.y*dim.x),min=-1,max=1),nrow=dim.y,ncol=dim.x)

        ## generate the current image with noise and signal
        cur.img <- noise
        cur.img[box.xs,box.ys] <- cur.img[box.xs,box.ys] + (sig[i]*sig.peak)

        ## save the current image
        img.list[[i]] <- cur.img

        i=i+1
    }

    return(img.list)
}
