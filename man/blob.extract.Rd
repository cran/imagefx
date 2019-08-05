\name{blob.extract}
\alias{blob.extract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Extract Blob Region from Image (Matrix)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Find the predominant blob region in an image using a Laplacian of Gaussian (LoG) blob detection algorithm.  Blob points are found using a connected component algorithm (see Details)
}
\usage{
blob.extract(img, blob.point, win.size, gaus, lap)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{img}{
%%     ~~Describe \code{img} here~~
Matrix with numeric values representing pixel color intensities
}
  \item{blob.point}{
%%     ~~Describe \code{blob.point} here~~
x,y locations of a point that is contained within the sought after blob region.  Normally the image's max (or min) value location.
}
  \item{win.size}{
%%     ~~Describe \code{win.size} here~~
Window size used in connected component algorithm (see Details).
}
  \item{gaus}{
%%     ~~Describe \code{gaus} here~~
Low pass Gaussian mask that has same dimensions as img
}
  \item{lap}{
%%     ~~Describe \code{lap} here~~
Optional high pass Laplacian filter of same dimensions as img.  Defaults to standard 5-point stencil.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The LoG algorithm first applies a Gaussian then a Laplacian operator to the image.  The resulting image is binarized (values either 0 or 1) depending on sign of values in the LoG Image.

The blob x,y locations surrounding the \code{blob.point} and are found via a connected component algorithm.  This algorithm is designed for speed and may miss x,y locations if the blob is highly irregular or concave.  Adjusting the \code{win.size} can yield more accurate blob locations but has a slower run time.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
List of length 2 where
\item{xy.coords}{Matrix of x,y locations of blob in image}
\item{bin.image}{Image (matrix) of same dimension of \code{img} that gives the binarized result from the LoG Blob Detection}
}

\author{
%%  ~~who you are~~
Alex J.C. Witsil
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{build.gaus}}
\code{\link{filt3d}}

}
\examples{
############
### EG 1 ###
############
## example with synthetic data

## create an image that is a simple gaussian
img <- build.gaus(100,100,sig.x=2,sig.y=10,x.mid=80,y.mid=60)

## find the where the maximum value is
img.max <- which(img==max(img),arr.ind=TRUE)

## define a sigma for the low pass filter
sig=5

## define the low pass filter as another gaussian
lp.filt <- build.gaus(nrow(img),ncol(img),sig.x=sig)

## define a window size for the connected component algorithm
win.size=0.05

## perform the blob detection
blob <- blob.extract(img=img, blob.point=img.max,win.size=win.size,gaus=lp.filt)

####################
### PLOTTING EG1 ###
####################

## define x and y grid lines
grid.x <- 1:nrow(img)
grid.y <- 1:ncol(img)

dev.new()
close.screen(all.screens=TRUE)
split.screen(c(1,3))

screen(1)
image(grid.x,grid.y,img,main='Image')

screen(2)
image(grid.x,grid.y,blob$bin.image,col=gray.colors(2),main='Binarized LoG Image')

screen(3)
image(grid.x,grid.y,img,main='Img with Blob Detected')
points(blob$xy.coords,col='black',pch=16,cex=1)

## close the screens
close.screen(all.screens=TRUE)


############
### EG 2 ###
############
## example with volcano image data.
## This RBG image shows ash erupting above the crater, which is cropped out

data(sakurajima)

## crop accroding to these corner values
xleft = 1
xright = 188
ybottom = 1
ytop = 396

## crop the image using crop.image
cropped <- crop.image(sakurajima, xleft, ybottom, xright, ytop)

## redefine the crop image
img <- cropped$img.crop

######################
### PRE PROCESSING ###
######################

## separate the image into red, green, and blue images
r.img <- img[,,1]
g.img <- img[,,2]
b.img <- img[,,3]

## remove the mean
r.img <- r.img-mean(r.img)
g.img <- g.img-mean(g.img)
b.img <- b.img-mean(b.img)

## calculate the the plane trend...
r.img.trend <- fit3d(r.img)
g.img.trend <- fit3d(g.img)
b.img.trend <- fit3d(b.img)

## remove the trend
r.img.dtrend <- r.img-r.img.trend
g.img.dtrend <- g.img-g.img.trend
b.img.dtrend <- b.img-b.img.trend


################################
### SET UP SOME FILTER MASKS ###
################################

## define a sigma for the LP Gaussian Filter
gaus.sig=15

## build the Gaussian filter
gaus <- build.gaus(nrow(img),ncol(img),gaus.sig)

## find the extreme (absolute valued maximum) value of each RGB channel
blob.r.point <- which(abs(r.img.dtrend)==max(abs(r.img.dtrend)),arr.ind=TRUE)
blob.g.point <- which(abs(g.img.dtrend)==max(abs(g.img.dtrend)),arr.ind=TRUE)
blob.b.point <- which(abs(b.img.dtrend)==max(abs(b.img.dtrend)),arr.ind=TRUE)

## set a window size to be used in the connected component algorithm
win.size = 0.05

## extract the blob xy locations
blob.r <- blob.extract(r.img.dtrend,blob.r.point,win.size,gaus)
blob.g <- blob.extract(g.img.dtrend,blob.r.point,win.size,gaus)
blob.b <- blob.extract(b.img.dtrend,blob.r.point,win.size,gaus)


####################
### PLOTTING EG2 ###
####################

## note the blob points (blob$xy.coords) must be adjusted according to
## where the origin (0,0) is located in R plots image plots
blob.coords.r  <- blob.r$xy.coords
blob.coords.r[,1] <- blob.r$xy.coords[,2]
blob.coords.r[,2] <- (blob.r$xy.coords[,1]-nrow(r.img))*-1

blob.coords.g  <- blob.g$xy.coords
blob.coords.g[,1] <- blob.g$xy.coords[,2]
blob.coords.g[,2] <- (blob.g$xy.coords[,1]-nrow(g.img))*-1

blob.coords.b  <- blob.b$xy.coords
blob.coords.b[,1] <- blob.b$xy.coords[,2]
blob.coords.b[,2] <- (blob.b$xy.coords[,1]-nrow(b.img))*-1


## save the users options
mar.usr=par()$mar

dev.new()
close.screen(all.screen=TRUE)
par(mar=c(0,0,2,0))
split.screen(c(1,2))
split.screen(c(3,1),screen=2)

screen(1)
image2(sakurajima,asp=1,axes=FALSE)
rect(ybottom,nrow(sakurajima)-xleft,ytop,nrow(sakurajima)-xright,lwd=3,border='white',lty=3)
title('Original Image',line=0,font=2,col='white',cex=2,)

screen(3)
image2(r.img,asp=1,axes=FALSE)
points(blob.coords.r,col=rgb(1,0,0,alpha=0.05),pch=16,cex=0.3)
title('Red Channel',line=0,font=2,col='red',cex=2)

screen(4)
image2(g.img,asp=1,axes=FALSE)
points(blob.coords.g,col=rgb(0,1,0,alpha=0.05),pch=16,cex=0.3)
title('Green Channel',line=0,font=2,col='darkgreen',cex=2)

screen(5)
image2(b.img,asp=1,axes=FALSE)
points(blob.coords.b,col=rgb(0,0,1,alpha=0.05),pch=16,cex=0.3)
title('Blue Channel',line=0,font=2,col='darkblue',cex=2)

## return the users original margins and close screens
par(mar=mar.usr)
close.screen(all.screens=TRUE)

}


