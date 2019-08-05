fit3d <- function(mat) {

    ## break into x,y,z matrix
    x <- as.vector(row(mat))
    y <- as.vector(col(mat))
    z <- as.vector(mat)

    ## what is the centroid
    x1 <- mean(x)
    y1 <- mean(y)
    z1 <- mean(z)

    ## put these data in a matrix of three columns
    data.mat <- rbind(x-x1,y-y1,z-z1)

    ## perform svd on data.mat
    data.svd <- svd(data.mat)

    ## find the normal vector to the plane
    ## this is the left singular vector corresponding to the least singular value
    plane.norm <- data.svd$u[,which.min(data.svd$d)]
  
    a <- plane.norm[1]
    b <- plane.norm[2]
    c <- plane.norm[3]

    ## find the coordinates of this plane along the rows and columns
    ## of the data matrix
    d = -a*x1 - b*y1 - c*z1
    plane.zs <- (-d - a*x - b*y) / c

    ## reformulate into a matrix
    plane.mat <- matrix(plane.zs,nrow=nrow(mat),ncol=ncol(mat),byrow=FALSE)
    
    return(plane.mat)
    
}
