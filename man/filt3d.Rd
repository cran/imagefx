\name{filt3d}
\alias{filt3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Filter Image (Matrix) with Mask via Convolution
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Apply a filter mask to smooth, sharpen, shift, or otherwise modify an image (matrix)
}
\usage{
filt3d(x, mask)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
Image (matrix) to be filtered
}
  \item{mask}{
%%     ~~Describe \code{mask} here~~
Filter mask (matrix) with same dimensions as \code{x}
}
}
\details{
\code{x} and \code{mask} are convolved in the frequency domain via multiplication and returned to the spatial domain using the fast Fourier transform.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Filtered matrix with same dimensions as \code{x}.
}
\author{
%%  ~~who you are~~
Alex J.C. Witsil
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{fft}}
\code{\link{fftshift}}
}
\examples{
##########
## EG 1 ##
##########
## example of a low pass filter

## generate test data
data <- matrix(0,nrow=256,ncol=256)

box.width = 100
box.height = 100
box.mid=c(nrow(data)/2,ncol(data)/2)

## define where the box indices are
box.row.inds <- (box.mid[1]-box.width/2):(box.mid[1]+box.width/2)
box.col.inds <- (box.mid[2]-box.height/2):(box.mid[2]+box.height/2)

## create the box in the data matrix
data[box.row.inds,box.col.inds] = 1

## define the sigma in the low pass Gaussian filter
sig=5

## create a low pass Gaussian filter
gaus <- build.gaus(nrow(data),ncol(data),sig)

## filter the data matrix with the Gaussian filter mask
data.lp <- filt3d(data,gaus)


## PLOTTING EG1 ##

dev.new()
close.screen(all.screens=TRUE)
split.screen(c(1,3))

## set up some grid lines
grid.xs <- 1:nrow(data)
grid.ys <- 1:ncol(data)

screen(1)
image(grid.xs,grid.ys,data,col=gray.colors(200),useRaster=TRUE,main="Data")

screen(2)
image(grid.xs,grid.ys,gaus,col=gray.colors(200),useRaster=TRUE,main="Low Pass Gaussian Mask")

screen(3)
image(grid.xs,grid.ys,data.lp,col=gray.colors(200),useRaster=TRUE,main='Filtered Data')

## close screens
close.screen(all.screens=TRUE)


##########
## EG 2 ##
##########
## example of a high pass filter

## generate test data
data <- matrix(0,nrow=256,ncol=256)

box.width = 100
box.height = 100
box.mid=c(nrow(data)/2,ncol(data)/2)

## define where the box indices are
box.row.inds <- (box.mid[1]-box.width/2):(box.mid[1]+box.width/2)
box.col.inds <- (box.mid[2]-box.height/2):(box.mid[2]+box.height/2)

## create the box in the data matrix
data[box.row.inds,box.col.inds] = 1

## find the middle of the data matrix
mid <- c(nrow(data)/2,ncol(data)/2)

## create a 5-point Laplacian high pass filter
lap <- matrix(0,nrow=nrow(data),ncol=ncol(data))
lap[(mid[1]-1):(mid[1]+1),mid[2]] = c(1,-4,1)
lap[mid[1],(mid[2]-1):(mid[2]+1)] = c(1,-4,1)

## perform  high pass filter on the data using the Laplacian mask
data.hp <- filt3d(data,lap)


## PLOTTING EG2 ##

## set up some grid lines
grid.xs <- 1:nrow(data)
grid.ys <- 1:ncol(data)

dev.new()
close.screen(all.screens=TRUE)
split.screen(c(1,3))

screen(1)
image(grid.xs,grid.ys,data,col=gray.colors(200),useRaster=TRUE,main="Data")

screen(2)
image(grid.xs,grid.ys,lap,col=gray.colors(200),useRaster=TRUE,main="High Pass Laplacian Mask")

screen(3)
image(grid.xs,grid.ys,data.hp,col=gray.colors(200),useRaster=TRUE,main='Filtered Data')

## close screens
close.screen(all.screens=TRUE)


##########
## EG 3 ##
##########
## example of a shift transform filter

## generate test data
data <- matrix(0,nrow=256,ncol=256)

box.width = 100
box.height = 100
box.mid=c(nrow(data)/2,ncol(data)/2)

## define where the box indices are
box.row.inds <- (box.mid[1]-box.width/2):(box.mid[1]+box.width/2)
box.col.inds <- (box.mid[2]-box.height/2):(box.mid[2]+box.height/2)

## create the box in the data matrix
data[box.row.inds,box.col.inds] = 1

## create a delta function at some (x,y) location
delta.x = 80
delta.y = 180
delta <- matrix(0,nrow=nrow(data),ncol=ncol(data))
delta[delta.x,delta.y] = 1

## perform the shift transform filter
data.shift <- filt3d(data,delta)


## PLOTTING EG3 ##

## set up some grid lines
grid.xs <- 1:nrow(data)
grid.ys <- 1:ncol(data)

dev.new()
close.screen(all.screens=TRUE)
split.screen(c(1,3))

screen(1)
image(grid.xs,grid.ys,data,col=gray.colors(200),useRaster=TRUE,main="Data")

screen(2)
image(grid.xs,grid.ys,delta,col=gray.colors(200),useRaster=TRUE,main="Shift Delta Mask")

screen(3)
image(grid.xs,grid.ys,data.shift,col=gray.colors(200),useRaster=TRUE,main='Filtered Data')

## close screens
close.screen(all.screens=TRUE)

}
