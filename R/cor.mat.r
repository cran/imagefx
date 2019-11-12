##################################
## ---- Correlation Matrix ---- ##
###                            ###
## Wrapper function of ccf whose##
## output returns only the best ##
## lag time associated highest  ##
## correlation vaule between two##
## vectors.                     ##
##################################

cor.mat <- function(x,y,...) {

    ## x ----> vector
    ## y ----> vecotr
    ## ... --> additional arguments to ccf (e.g. max.lag)

    ## calculate the current correlation
    cur.corr <- ccf(x,y,plot=FALSE,...)

    ## find the maximum correlation value
    max.corr <- which.max(cur.corr$acf)

    ## and the associated lag time
    best.lag <- cur.corr$lag[max.corr]

    return(best.lag)
}
