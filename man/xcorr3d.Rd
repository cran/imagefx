\name{xcorr3d}
\alias{xcorr3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Normalized Cross Correlation of Images 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Input two images (matrices) and perform normalized cross correlation by multiplication in the frequency domain.  Return the maximum normalized cross correlation value, its associated shift vector (x and y), and the correlation matrix.
}
\usage{
xcorr3d(img1,img2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{img1}{
  Image (matrix) 1
  }
  \item{img2}{
  Image (matrix) 2 with same dimensions of \code{img1} 
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Correlation calculated in the frequency domain as a multiplication.  The dimensions of \code{img1} and \code{img2} must match.  If \code{xcorr3d} is used to apply a match filter, it is logical to input the image to be searched over as \code{img1} and the match filter as \code{img2}.  Similarly, if tracking relative motion between images, it is logical to input the first image at time t=n as \code{img1} and the second image at time t=n+1 as \code{img2}, otherwise motions will backwards.  
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
List whose values correspond to:
\item{max.shifts}{Vector of length two whose values are the x and y indices associated with the highest correlation value.  Note these values are shifted according to the zero frequency which changes depending on the dimensions of \code{img1} and/or \code{img2}.}
\item{max.corr}{Highest normalized correlation value in the correlation matrix}
\item{corr.mat}{Normalized correlation matrix}

}
\author{
%%  ~~who you are~~
Alex J.C. Witsil
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{pcorr3d}}
}
\examples{

############################
### Optical Flow Example ###
############################
## track movement between images

## set up the image domain
xdim = 256
ydim = 256

## shift vectors accounting for movement between the two images
x.vec = 100
y.vec = 100

## declare the indices where the Gaussian peak will be in the first image
gaus1.x = 50
gaus1.y = 50

## shift the Gaussian according to the shift vector
gaus2.x <- gaus1.x + x.vec
gaus2.y <- gaus1.y + y.vec

## create the first synthetic image
img1 = build.gaus(xdim,ydim,sig.x=10,x.mid=gaus1.x,y.mid=gaus1.y)

## create the second synthetic image
img2 = build.gaus(xdim,ydim,sig.x=10,x.mid=gaus1.x+x.vec,y.mid=gaus1.y+y.vec)

## now find the shift vector by using the cross correlation function 
shifts <- xcorr3d(img1,img2)

#############################
### Plotting Optical Flow ###
#############################

split.screen(c(1,2))
screen(1)
image(1:xdim,1:ydim,img1)

## add arrows indicating how the image shifted
arrows(gaus1.x,gaus1.y,gaus1.x+shifts$max.shifts[1],gaus1.y+shifts$max.shifts[2])

## add a point where the arrow is
points(gaus1.x+shifts$max.shifts[1],gaus1.y+shifts$max.shifts[2],pch=21,bg='green')

screen(2)
image(1:xdim,1:ydim,img2)

## add the point showing where the arrow is pointing
points(gaus1.x+shifts$max.shifts[1],gaus1.y+shifts$max.shifts[2],pch=21,bg='green')

## close the screen
close.screen(all.screens=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
