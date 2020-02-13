\name{fftshift}
\alias{fftshift}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Shift Zero Frequency to Center
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Rearranges matrix such that the first quadrant is swapped with the third and the second quadrant is swapped with the fourth.
}
\usage{
fftshift(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
matrix whose quadrants should be shifted.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This function is generally used after applying an \code{fft} to force the zero-frequency to the middle of the matrix.

Note that if the matrix \code{x} has even dimensions, the zero frequency will be 1 unit from the center.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Shifted matrix with same dimensions as \code{x}
}
\author{
%%  ~~who you are~~
Alex J.C. Witsil
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{fft}}
}
\examples{

## build the four components of a matrix with four values (i.e. 1:4)
x1 <- matrix(1,nrow=1,ncol=1)
x2 <- x1+1
x3 <- x2+1
x4 <- x3+1

## combine all components together
x <- rbind(cbind(x1,x2),cbind(x3,x4))

## shift the matrix
x.shift <- fftshift(x)

## note the difference of the shifted and original
print(x)
print(x.shift)

################
### PLOTTING ###
################
## note the difference of the shifted and original graphically

close.screen(all.screens=TRUE)
split.screen(c(1,2))
screen(1)
image(x,main='Original',col=rainbow(4))
screen(2)
image(x.shift,main='FFT Shifted', col=rainbow(4))

## close screens
close.screen(all.screens=TRUE)

}
