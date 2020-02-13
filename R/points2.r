########################
## ---- Points 2 ---- ##
###                  ###
## Plot Points to go  ##
## with the image2 fu-##
##-nction.            ##
########################


points2 <- function(x,y,img,...) {

    plot.xs <- y
    plot.ys <- (x - nrow(img))*-1

    points(plot.xs,plot.ys,...)
}


    
    
    
