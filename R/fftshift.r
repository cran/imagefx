fftshift <- function(x) {
    
        rd2 <- floor(nrow(x)/2)
        cd2 <- floor(ncol(x)/2)

        q1 <- x[1:rd2,1:cd2]
        q2 <- x[1:rd2,(cd2+1):ncol(x)]
        q3 <- x[(rd2+1):nrow(x),(cd2+1):ncol(x)]
        q4 <- x[(rd2+1):nrow(x),1:cd2]

        centered.t <- rbind(q4,q1)
        centered.b <- rbind(q3,q2)
        centered <- cbind(centered.b,centered.t)

        return(Re(centered))
    
}

