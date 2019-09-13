\name{pcorr3d}
\alias{pcorr3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Phase Correlation of Images 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Input two images (matrices) and perform phase correlation by multiplication in the frequency domain.  Return the maximum phase correlation value, its associated shift vector (x and y), and the phase correlation matrix.
}
\usage{
pcorr3d(img1,img2)
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
Phase correlation calculated in the frequency domain as a multiplication.  The dimensions of \code{img1} and \code{img2} must match.  If \code{pcorr3d} is used to apply a match filter, it is logical to input the image to be searched over as \code{img1} and the match filter as \code{img2}.  Similarly, if tracking relative motion between images, it is logical to input the first image at time t=n as \code{img1} and the second image at time t=n+1 as \code{img2}, otherwise motions will backwards.  
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
List whose values correspond to:
\item{max.shifts}{Vector of length two whose values are the x and y indices associated with the highest phase correlation value.  Note these values are shifted according to the zero frequency which changes depending on the dimensions of \code{img1} and/or \code{img2}.}
\item{max.corr}{Highest normalized phase correlation value in the correlation matrix}
\item{corr.mat}{Normalized phase correlation matrix}

}
\author{
%%  ~~who you are~~
Alex J.C. Witsil
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{xcorr3d}}
}
\examples{

#################
### Example 1 ###
#################
## track movement between images

## load in the images
data(tux1,tux2)

## now find the shift vector by using the phase correlation function 
shifts <- pcorr3d(tux1,tux2)

## ---- Plotting Example 1  ----- ##

split.screen(c(1,2))
screen(1)
image(1:nrow(tux1),1:ncol(tux1),tux1,col=gray.colors(200))

## define an example arrow starting and end points based on the shift found
x0 = nrow(tux1)/2
y0 = ncol(tux1)/2
x1 = x0 + shifts$max.shifts[1]
y1 = y0 + shifts$max.shifts[2]

## add arrows indicating how the image shifted
arrows(x0,y0,x1,y1)

## add a point where the arrow is
points(nrow(tux1)/2+shifts$max.shifts[1],ncol(tux1)/2+shifts$max.shifts[2],pch=21,bg='green')

screen(2)
image(1:nrow(tux2),1:ncol(tux2),tux2,col=gray.colors(200))
points(nrow(tux1)/2+shifts$max.shifts[1],ncol(tux1)/2+shifts$max.shifts[2],pch=21,bg='green')

## close the screen
close.screen(all.screens=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
