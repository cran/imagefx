\name{fit3d}
\alias{fit3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Fit a Plane to Image (Matrix) with SVD
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Find plane that best fits trend in image (matrix)
}
\usage{
fit3d(mat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
%%     ~~Describe \code{mat} here~~
image (matrix) of values to fit plane to
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This function returns the best fit plane with the DC offset included.  In other words average of the matrix values is added to the best fit plane within the function.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
matrix with same dimensions as \code{mat} whose values represent the best fit plane.
}
\author{
Alex J.C. Witsil
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{svd}}
}
\examples{

## break the RGB image into 3 matrices
img.r <- erebus[,,1]
img.g <- erebus[,,2]
img.b <- erebus[,,3]

## find the planar trend in the red channel
trend.r <- fit3d(img.r)
trend.g <- fit3d(img.g)
trend.b <- fit3d(img.b)

## subtract the red channel trend from the original red channel image
img.r.detrend <- img.r-trend.r
img.g.detrend <- img.g-trend.g
img.b.detrend <- img.b-trend.b

## combine the RGB detrended matrices into an array
img.detrend = array(dim=dim(erebus))
img.detrend[,,1] <- img.r.detrend
img.detrend[,,2] <- img.g.detrend
img.detrend[,,3] <- img.b.detrend

################
### PLOTTING ###
################

close.screen(all.screens=TRUE)
split.screen(c(1,3))
screen(1)
image2(erebus,asp=1,main='Original Image',ylab='image rows',xlab='image cols')
screen(2)
image2(trend.r,asp=1,main='Fitted Trend',ylab='image rows',xlab='image cols')
screen(3)
image2(img.detrend,asp=1,main='Detrended Image',ylab='image rows',xlab='image cols')

## close screens
close.screen(all.screens=TRUE)

}
