\name{build.gaus}
\alias{build.gaus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Build 2D Gaussian Image (Matrix)
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Build a 2-dimensional Gaussian matrix for filtering, correlations, data testing, or other various uses.
}
\usage{
build.gaus(xdim, ydim, sig.x, sig.y, x.mid, y.mid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xdim}{
%%     ~~Describe \code{xdim} here~~
size in the x dimension
}
  \item{ydim}{
%%     ~~Describe \code{ydim} here~~
size in the y dimension
}
  \item{sig.x}{
%%     ~~Describe \code{sig.x} here~~
Gaussian sqrt(variance) in x direction
}
  \item{sig.y}{
%%     ~~Describe \code{sig.y} here~~
Gaussian sqrt(variance) in y direction.  Defaults to \code{sig.x} if undefined
}
  \item{x.mid}{
%%     ~~Describe \code{x.mid} here~~
peak location in x direction
}
  \item{y.mid}{
%%     ~~Describe \code{y.mid} here~~
peak location in the y direction
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Note that if \code{xdim} or \code{ydim} are even and \code{x.mid} and \code{y.mid} are left undefined, the Gaussian peak will be off center.  This can be a problem when using a Gaussian matrix for multiple filtering operations.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
matrix with values corresponding to values in the 2D Gaussian.
}
\author{
%%  ~~who you are~~
Alex J.C. Witsil
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

############
### EG 1 ###
############

## define the dimensions of the gaussian image (matrix)
xdim=101
ydim=101

## sigma in the x direction.  The y sigma defaults to the sig.x if not supplied
sig.x=5

## build the first example
gaus1 <- build.gaus(xdim,ydim,sig.x)


###################
## PLOTTING EG 1 ##
###################
image(1:nrow(gaus1),1:ncol(gaus1),useRaster=TRUE,gaus1)


############
### EG 2 ###
############

## define the dimensions of the gaussian image (matrix)
xdim=101
ydim=201

## define a sigma in the both the x and y direction
sig.y=5
sig.y=20

## define the center (peak) location of the guassian
x.mid = 30
y.mid = 120

## now build the gaussian
gaus2 <- build.gaus(xdim,ydim,sig.x,sig.y,x.mid,y.mid)

##################
## PLOTTING EG2 ##
##################
image(1:nrow(gaus2),1:ncol(gaus2),useRaster=TRUE,gaus2)

}
