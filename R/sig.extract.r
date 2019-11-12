sig.extract <- function(img.list, fps, base.vec, ...) {

    ## transform the image list to a matrix with columns of pixels and rows of time
    img.series <- matrix(unlist(img.list),ncol=length(img.list[[1]]),byrow=TRUE)

    ## if missing anything
    if(missing(base.vec)) { base.vec <- rowMeans(img.series) }
    if(missing(fps)) { fps = 30 }

    ## generate an initial stack of row sums to later compare to the shifted stack
    org.sig.all <- rowSums(img.series)

    ## calculate the correlation between all pixel image series and the base vector
    series.lags <- apply(img.series,2,cor.mat,y=base.vec,...)
        
    ## initilize a matrix to hold the shifted series
    shift.series <- matrix(NA,nrow=nrow(img.series),ncol=ncol(img.series))

    ## shift the image series according to the series lags
    ## do this in a loop for now
    i=1
    while(i<=ncol(img.series)) {

        ## shift the current imgage series according to the best lag
        shift.series[,i] <- shift.vec(series.lags[i],img.series[,i])

        i=i+1
    }

    ## note there will be NAs (probably), which we remove later
    shifted.sig.all <- rowSums(shift.series)

    ## remove all NAs from the image series stack and original
    shifted.sig <- shifted.sig.all[!is.na(shifted.sig.all)]
    org.sig <- org.sig.all[!is.na(shifted.sig.all)]

    
    ## ################ ##
    ## FREQUENCY DOMAIN ## 
    ## ################ ##
    ## for both the original and the shifted signals

    ## what is the time and frequency spacing
    dt <- 1/fps
    df <- fps/length(shifted.sig)

    ## make some time and frequency axis
    tax <- (0:(length(shifted.sig.all)-1))*dt
    fax <- (0:(length(shifted.sig)-1))*df
    
    ## remove the mean
    org.sig.dmean <- org.sig - mean(org.sig)
    shifted.sig.dmean <- shifted.sig-mean(shifted.sig)
    
    ## go into the frequency domain
    SHIFTED.SIG  <- abs(fft(shifted.sig.dmean))
    ORG.SIG <- abs(fft(org.sig.dmean))

    ## ###### ##
    ## SAVING ## 
    ## ###### ##
    
    ## save everything to a list
    sig=list()

    ## add the time domain
    sig$org <- cbind(tax,org.sig.all)
    sig$ORG <- cbind(fax,ORG.SIG)

    ## and the frequency domain
    sig$shifted <- cbind(tax,shifted.sig.all)
    sig$SHIFTED <- cbind(fax,SHIFTED.SIG)

    return(sig)

}
