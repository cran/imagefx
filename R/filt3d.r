filt3d <- function(x,mask) {
    x.filt <- fft((fft(x)*fft(mask)),inverse=TRUE)/length(x)
    x.filt <- fftshift(Re(x.filt))

    return(x.filt)
}


