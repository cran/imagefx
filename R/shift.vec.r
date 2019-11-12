##############################
## ---- Shift a Vector ---- ##
###                        ###
## Given a vector and delay ##
## time, shift that vector  ##
##############################

shift.vec <- function(shift,vec) {
    n <- length(vec)
    vec.new <- rep(NA, n)
    if (shift < 0) {
        vec.new[1:(n-abs(shift))] <- vec[(abs(shift)+1):n]
    } else if (shift > 0) {
        vec.new[(shift+1):n] <- vec[1:(n-shift)]
    } else {
        vec.new <- vec
    }
    return(vec.new)
}
