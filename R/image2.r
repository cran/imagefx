image2 <- function(img,...) {
    
    img01 <- range01(img)
    img.rast <- as.raster(img01)
   
    plot(0,0,type='n',xlim=c(1,ncol(img.rast)),ylim=c(1,nrow(img.rast)),...)
    rasterImage(img.rast,xleft=1,ybottom=1,xright=ncol(img.rast),ytop=nrow(img.rast))
}
