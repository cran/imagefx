##############################
## ---- Amplify Signal ---- ##
###                        ###
## Time filter a series of  ##
## images, enhance the band-##
## passed signal and add it ##
## back to the images       ##
##############################

amp.sig <- function(img.list, fps, f.corners, amp ,n.poles, type) {

##############
### INPUTS ###
##############
    
    ## img.list --------------> list whose elements correspond to video frames
    ## fps -------------------> sample rate of video in frames per second
    ## amp -------------------> scalar to amp the signal by
    ## n.poles ---------------> number of poles in butterworth filter
    ## type ------------------> type of filter (i.e. low, high, pass, stop)


########################
### MISSING ANYTHING ###
########################

    if(missing(fps)) {fps=30}
    if(missing(amp)) { amp=10}
    if(missing(n.poles)) { n.poles=2 }
    if(missing(type)) {
        if(length(f.corners)==2) {type='pass'}
        if(length(f.corners)==1) {type='high'}
    }


######################
### PRE PROCESSING ###
######################    
    
    ## transform the image list to a matrix with columns of pixels and rows of time
    img.series <- matrix(unlist(img.list),ncol=length(img.list[[1]]),byrow=TRUE)

    ## find the mean of the image series
    img.series.mean <- matrix(rep(colMeans(img.series),nrow(img.series)),ncol=ncol(img.series),nrow=nrow(img.series),byrow=TRUE)

    ## subtract the mean from the image series
    img.series.dmean <- img.series - img.series.mean

    
#################
### FILTERING ###
#################

    ## given the frames per second, what is the nyquist frequency
    nyq  <- 1/2 * fps 

    ## create the butterworth filter coeff. with the corners
    filt.coef <- butter(n.poles, W=f.corners/nyq, type='pass')

    ## filter the image series using filtfilt
    filt.img.series <-  apply(img.series.dmean,2,filtfilt,filt=filt.coef)

    
######################
### AMPLIFY SIGNAL ###
######################

    ## scale the filtered image series then add the mean (DC) back to it
    amp.img.series <- (filt.img.series*amp)+img.series.mean

    ## create a list to hold the amplified image signals
    amp.img.list=NULL

    ## get the filtered image series back to a list
    i=1
    while(i<=length(img.list)) {

        ## find the dimensions of the original image
        img.rows <- nrow(img.list[[i]])
        img.cols <- ncol(img.list[[i]])

        ## get the amplified image series into a list
        amp.img.list[[i]] <- matrix(amp.img.series[i,],nrow=img.rows,ncol=img.cols,byrow=FALSE)

        i=i+1
    }

    ## return the amplify image list
    return(amp.img.list)
}

    


        
