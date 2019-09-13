pcorr3d <- function(img1,img2) {
    
    ## go to the frequency domain and take conjugate of first image 
    IMG1c <- Conj(fft(img1))
    IMG2 <- fft(img2)

    ## calculate the phase power spectrum
    R <- (IMG1c * IMG2)/abs(IMG1c * IMG2)

    ## back to the spatial domain
    r.shift <- fft(R,inverse=TRUE)/length(R)

    ## rearrange so zero frequency is in middle
    r <- fftshift(Re(r.shift))

    ## what is the correlation value associated with best shift
    max.cor <- max(r)

    ## find where the zero frequency is
    if(nrow(r)%%2 == 1) { zero.freq.x = -1.5 }
    if(nrow(r)%%2 == 0) { zero.freq.x = -1   }
    if(ncol(r)%%2 == 1) { zero.freq.y = -1.5 }
    if(ncol(r)%%2 == 0) { zero.freq.y = -1   }

    ## find the maximum value relative to the middle
    max.inds.abs <- which(r==max.cor,arr.ind=TRUE)-(dim(r)/2)

    ## adjust the maximum indices according to the true zero frequency
    max.inds <- max.inds.abs + c(zero.freq.x,zero.freq.y)

    ## if the input images are made of 0s, the r matrix could be filled
    ## with NaN and there is no max
    if(length(max.inds)==0){max.inds=matrix(c(0,0),nrow=1)}

    ## sometimes there is more than one max (normally in blank images)
    ## so take only the first max ind
    max.inds <- max.inds[1,]

    ## create a list to hold the max correlation value, its indicies after 
    ## shifting according to the zero frequency, and the original correlation matrix
    return.list = list()
    return.list$max.shifts <- max.inds
    return.list$max.corr <- max.cor
    return.list$corr.mat <- r
    
    return(return.list)
}
