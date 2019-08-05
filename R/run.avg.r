run.avg <- function(data,win) {

    ## find the middle of the data vector
    data.mid <- length(data)/2

    ## make an averaging filter according to the window length
    ## this mask looks like this:  ^\_________/^ (has maxs at edges)
    mask <- rep(0,length.out=length(data))
    mask[1:win/2] <- 1/win
    mask <- rev(mask)
    mask[1:win/2] <- 1/win
    mask <- rev(mask)

    ## if window length is odd, then mask will not sum to 0
    if(win%%2 != 0) {
        mask <- mask/sum(mask)
    }

    ## filter (or smooth)
    smoothed <- Re(fft(fft(data) * fft(mask), inverse=TRUE)/length(data))

    return(smoothed)
}


