blob.extract <- function(img,blob.point,win.size, gaus, lap) {

    ## if the point is an integer add a fraction to it so that we have only triangles and no lines
    fraction=0.5
    if(any(blob.point %% 1 == 0)) {
        blob.point <- blob.point + fraction
        }

    ## first build a high pass filter if one isn't given
    if(missing(lap)) {
        lap <- matrix(0,nrow(img),ncol(img))
        lap.mid.x <- floor(nrow(lap)/2)
        lap.mid.y <- floor(ncol(lap)/2)
        ## now make the standard 5-point laplacian
        lap[(lap.mid.x-1):(lap.mid.x+1),(lap.mid.y-1):(lap.mid.y+1)] = c(0,1,0,1,-4,1,0,1,0)
        }

    ## perform a low pass filter on the image
    lp <- filt3d(img,gaus)

    ## take the absolute value of the LP iamge
    lp.abs <- abs(lp)

    ## Perform a high pass filter on the image
    hp <- filt3d(lp.abs,lap)

    ## make a binary image with the negative slope
    bin <- matrix(0,nrow(hp),ncol(hp))
    bin[which(hp<0,arr.ind=TRUE)] = 1
    bin[which(hp>=0,arr.ind=TRUE)] = 0

    ## define the x and y points to transform to polar coordinates
    pxs <- rep(1:nrow(bin),ncol(bin))
    pys <- rep(1:ncol(bin),each=nrow(bin))

    ## reference the points to the origin
    px.ref <- pxs-blob.point[1]
    py.ref <- pys-blob.point[2
]
    ## cartesian to polar transormation
    theta <- atan(py.ref/px.ref)
    mag <- sqrt(py.ref^2+px.ref^2)

    ## there are ambiguities between the thetas and mags
    theta[which(px.ref>0 & py.ref > 0)] <- theta[which(px.ref>0 & py.ref > 0)] - pi/2
    theta[which(px.ref<0 & py.ref > 0)] <- theta[which(px.ref<0 & py.ref > 0)] + pi/2
    theta[which(px.ref<0 & py.ref < 0)] <- theta[which(px.ref<0 & py.ref < 0)] + pi/2
    theta[which(px.ref>0 & py.ref < 0)] <- theta[which(px.ref>0 & py.ref < 0)] + 3/2*pi

    ## get rid on any spurious NaN values
    if(any(is.nan(theta))) {
        theta[which(is.nan(theta))] = 0
    }

    ## put the thetas in a matrix ordered by the thetas (min to max)
    polar.coords <- cbind(theta,mag,as.vector(bin))
    polar.coords <- polar.coords[order(polar.coords[,1]),]

    ## determine the value of the blob in the binary image
    ## will vary depending if blob goes from dark to bright or vice versa
    blob.value <- bin[blob.point]
    not.blob.value <- abs(blob.value-1)

    ## use the window size to define a window
    win=c(min(theta),min(theta+win.size)) # theta

    ## initilize a matrix to hold the blob indices
    blob.mat <- matrix(NA,nrow=nrow(polar.coords),ncol=ncol(polar.coords))

    ## we need to know when to stop the loop because our window size is dynamic at the last iteration
    we.are.done=FALSE

    ## loop over the thetas according to the window

    while(win[2]<=max(theta)) {

        ##what are the current window indices
        win.inds <- which(polar.coords[,1] >= win[1] & polar.coords[,1] <= win[2])

        ## sometimes the window indices are too small and there are no points
        ## in the window so advance to the next iteration
        if(length(win.inds) == 0) {
            win=win+win.size
            next
        }


        ## define the current window based on the current window indices
        cur.win <- matrix(polar.coords[win.inds,],ncol=3)

        ## first find out if there are any 'not blob' values equal to 0
        if(any(cur.win[,3]==0)) {

            ## find those values in the current window that are not blob values
            not.blob <- matrix(cur.win[which(cur.win[,3]==not.blob.value),],ncol=3)

            ## find the lowest magnitude not blob value
            not.blob.min <- not.blob[which.min(not.blob[,2]),2]

            ## the value beneath this is the highest mag/distance of the blob
            ## at this angle window.
            cur.blob <- cur.win[which(cur.win[,2]<not.blob.min),]
        } else {cur.blob=cur.win}

        ## just in case the current blob matrix isn't a matrix
        cur.blob <- matrix(cur.blob,ncol=3)

        ## if there is a current blob matrix, save that data
        ## sometimes there isn't one and thats okay
        if(length(cur.blob)==0){
            ## we will remove NA values (which there will be regardless) later
            cur.blob <- matrix(NA,ncol=3)
        }

        ## save the current blob to the blob matrix
        blob.mat[win.inds[1:nrow(cur.blob)],] <- cur.blob

        ## if our window size doesn't divide into the maximum theta evenly we will miss
        ## the last few polar coordiantes.  Therefore dynamically change the window size
        ## as we get to the end of the loop.  This should only happen once
        if(win[2]+win.size >= max(theta)){
            ## check to if we have changed the window size before
            if(we.are.done==TRUE) {
                break
            }

            win.size <- max(theta) - win[2]
            ## indicate that we only have one more iteration to loop over
            we.are.done=TRUE
        }

        ## move the window forward
        win=win+win.size

    }

    ## remove all NA values in blob matrix
    blob.mat <- blob.mat[-which(is.na(blob.mat[,1])),]

    ## now lets map these back to cartesian coordinates
    blob.cart.x <- blob.mat[,2]*cos(blob.mat[,1]+pi/2) + blob.point[1]+fraction
    blob.cart.y <- blob.mat[,2]*sin(blob.mat[,1]+pi/2) + blob.point[2]+fraction

    blob.coords <- cbind(blob.cart.x,blob.cart.y)

    ## add the binarized image to the output
    output <- list()
    output[[1]] = blob.coords
    output[[2]] = bin

    names(output) <- c('xy.coords','bin.image')

    return(output)

}





