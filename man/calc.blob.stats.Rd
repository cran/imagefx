\name{calc.blob.stats}
\alias{calc.blob.stats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculate Color and Spatial Statistics from Blob Region
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Wrapper function (see details) that calculates various statistics on x,y data that corresponds to an image.

}
\usage{
calc.blob.stats(img, xy.coords)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{img}{
%%     ~~Describe \code{img} here~~
Matrix whose values at \code{xy.coords} are statistically analyzed.
}
  \item{xy.coords}{
%%     ~~Describe \code{xy.coords} here~~
Index locations corresponding region of interest (e.g. blob region) in the \code{img}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Function calls multiple statistical functions (e.g. \code{mean}, \code{sd}) and applies them to regions in the \code{img} according to the index locations given by \code{xy.coords}.  In general, this function is commented to promote any modifications needed to fit the users needs.  For example, adding or removing statistical analyses is straight forward.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Numeric vector giving the statistics of the blob region.
}
\author{
%%  ~~who you are~~
Alex J.C. Witsil
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{mean}}
\code{\link{sd}}
\code{\link{sum}}
\code{\link{colMeans}}
\code{\link{rowMeans}}
\code{\link{length}}
\code{\link[moments]{skewness}}
\code{\link[moments]{kurtosis}}
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

#################################
### CALCULATE BLOB STATISTICS ###
#################################

blob.stats <- calc.blob.stats(img, blob$xy.coords)
print(blob.stats)


############
### EG 2 ###
############

## example with volcano image data.
data(sakurajima)

######################
### PRE PROCESSING ###
######################

## crop accroding to these corner values
xleft = 1
xright = 188
ybottom = 1
ytop = 396

## crop the image using crop.image
cropped <- crop.image(sakurajima, xleft, ybottom, xright, ytop)

## redefine the crop image
img <- cropped$img.crop

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
gaus.sig=30

## build the Gaussian filter
gaus <- build.gaus(nrow(img),ncol(img),gaus.sig)

## find the maximum value of each RGB channel
blob.r.point <- which(r.img.dtrend==max(r.img.dtrend),arr.ind=TRUE)
blob.g.point <- which(g.img.dtrend==max(g.img.dtrend),arr.ind=TRUE)
blob.b.point <- which(b.img.dtrend==max(b.img.dtrend),arr.ind=TRUE)

## set a window size to be used in the connected component algorithm
win.size = 0.05

## extract the blob xy locations
blob.r <- blob.extract(r.img.dtrend,blob.r.point,win.size,gaus)
blob.g <- blob.extract(g.img.dtrend,blob.r.point,win.size,gaus)
blob.b <- blob.extract(b.img.dtrend,blob.r.point,win.size,gaus)


#################################
### CALCULATE BLOB STATISTICS ###
#################################

r.blob.stats <- calc.blob.stats(r.img.dtrend, blob.r$xy.coords)
g.blob.stats <- calc.blob.stats(g.img.dtrend, blob.g$xy.coords)
b.blob.stats <- calc.blob.stats(b.img.dtrend, blob.b$xy.coords)

print(r.blob.stats)
print(g.blob.stats)
print(b.blob.stats)

}
