calc.blob.stats <- function(img,xy.coords) {

    ## ---------------- ##
    ## COLOR STATISITCS ##
    ## vvvvvvvvvvvvvvvv ##

    ## BLOB SUM (after removing mean)
    blob.sum <- sum(img[xy.coords])

    ## BLOB AVERAGE COLOR ##
    blob.mean <- mean(img[xy.coords])

    ## BLOB COLOR STANDARD DEVIATION
    blob.col.sd <- sd(img[xy.coords])

    ## BLOB COLOR SKEWNESS
    blob.col.skew <- moments::skewness(img[xy.coords])

    ## BLOB COLOR KURTOSIS
    blob.col.kurt <- moments::kurtosis(img[xy.coords])

    ## ^^^^^^^^^^^^^^^^^^ ##


    ## ------------------ ##
    ## SPATIAL STATISTICS ##
    ## vvvvvvvvvvvvvvvvvv ##

    ## BLOB SIZE ##
    blob.size <- nrow(xy.coords)

    ## BLOB CENTROID (X,Y)
    blob.cent <- colMeans(xy.coords)

    ## BLOB POSITION STANDARD DEVIATION (X,Y)
    blob.pos.sd <- apply(xy.coords,2,sd)

    ## BLOB POSITION SKEWNESS (X,Y)
    blob.pos.skew <- apply(xy.coords,2,moments::skewness)

    ## BLOB POSITION KURTOSIS (X,Y)
    blob.pos.kurt <- apply(xy.coords,2,moments::kurtosis)

    ## ^^^^^^^^^^^^^^^^^^ ##


    ## add all the current statistics together
    blob.stats <- c(blob.sum,blob.mean,blob.col.sd,blob.col.skew, blob.col.kurt, blob.size, blob.cent, blob.pos.sd, blob.pos.skew, blob.pos.kurt)

    ## make a name vector for the current statistic columns
    stat.names = c('col_sum',
                   'col_mean',
                   'col_sd',
                   'col_skew',
                   'col_kurt',
                   'blob_area',
                   'cent_x','cent_y',
                   'pos_sd_x','pos_sd_y',
                   'pos_skew_x','pos_skew_y',
                   'pos_kurt_x','pos_kurt_y'
                   )

    ## name the img stats matrix
    names(blob.stats) <- stat.names

    return(blob.stats)
}

