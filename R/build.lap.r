##########################################
## ---- BUILD Five Point Laplacian ---- ##
##########################################

build.lap <- function(xdim,ydim) { 
    lap <- matrix(0, xdim, ydim)
    lap.mid.x <- ceiling(nrow(lap)/2)
    lap.mid.y <- ceiling(ncol(lap)/2)
    lap[(lap.mid.x - 1):(lap.mid.x + 1), (lap.mid.y - 1):(lap.mid.y + 1)] = c(0, 1, 0, 1, -4, 1, 0, 1, 0)

    return(lap)
}
