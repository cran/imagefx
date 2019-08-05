\name{run.avg}
\alias{run.avg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Perform Running Average via Convolution
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Smooth input data by convolution it with with a boxcar function of specified width.  This is done in the frequency domain using multiplication.
}
\usage{
run.avg(data, win)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
signal (vector) to convolve.
}
  \item{win}{
%%     ~~Describe \code{win} here~~
width of the boxcar in samples.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Convolution occurs in the frequency domain via a multiplication.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Smoothed vector with the same dimension as \code{data}
}
\author{
%%  ~~who you are~~
Alex J.C. Witsil
}
\note{
%%  ~~further notes~~
This function uses \code{fft} which can take significantly long run times if the input signal is prime or is divisible by few integers.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{fft}}
\code{\link{fftshift}}
}
\examples{
## make a delta 2D (time series) function
delta <- rep(0,101)
delta[floor(length(delta)/2)] = 1

## define a window length to average over
win = 20

## filter the delta function...this will result in a boxcar
box.car <- run.avg(delta,win)

## note sum(box.car) should equal the sum of the original signal...
## in this case sum(box.car)==1

##############
## PLOTTING ##
##############

plot(delta,type='h',lwd=2)
lines(box.car,col='blue',lwd=2,type='h')

legend('topright',col=c('black','blue'),legend=c('delta','running average'),lwd=2)
}
