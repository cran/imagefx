\name{sig.extract}
\alias{sig.extract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Time-Series Signal from Video Data
}
\description{
This function investigates time variations of pixel values in video frames via both a simple stack and also a cross correlation analysis.  Both methods attempt to highlight signals present within video data. See Details.  
}
\usage{
sig.extract(img.list, fps, base.vec, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{img.list}{
  A series of images saved in a list.  Each list element is a matrix that represents pixel values of an image, the dimensions of which correspond to the rows and columns of the matrix.
  }
  \item{fps}{
  Sample rate of video in frames per second (FPS).  Each list element of \code{img.list} should be separated by \code{1/fps} seconds. Defaults to 30. 
  }
  \item{base.vec}{
  Vector to correlate all pixel signals with.  Defaults to \code{rowMeans(img.series)}.
}
  \item{\dots}{
  Additional arguments passed to ccf (i.e., \code{lag.max}).
}
}
\details{
The algorithm first synthesizes the video frames into a matrix of time series signals. In other words, a time-series is generated for each pixel that records the value of that pixel in each frame.  These original pixel time-series are then simply stacked and then analyzed in terms of their frequency components.  Additionally, each pixel time-series is cross correlated with the \code{base.vec} and then shifted according to the lag associated with the maximum correlation value.  These shifted time-series are then stacked and analyzed in terms of their frequency components.  

Note the function gives two basic outputs.  The first is a simple stack (without applying any shift to the pixel time-series), while the second applies a shift then stack.  Both outputs may prove useful and should be investigated. 
}
\value{
List of length four whose elements correspond to both original and shifted stacks.  Note the nomenclature is that lower case objects correspond to time-series data while upper case corresponds to frequency domain data.  
\item{org }{Matrix of stacked time-series from original pixel values.  The first column corresponds to the time axis while second column is the stacked values.}
\item{ORG }{Matrix of stacked frequency components from original pixel values.  The first column corresponds to the frequency axis while second column is the stacked frequency amplitudes.}
\item{shifted }{Matrix of stacked time-series from shifted pixel values.  The first column corresponds to the time axis while second column is the stacked values.}
\item{SHIFTED }{Matrix of stacked frequency components from shifted pixel values.  The first column corresponds to the frequency axis while second column is the stacked frequency amplitudes.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Alex J.C. Witsil
}
\seealso{
\code{\link{gen.eg.img.list}}
}
\examples{

##############################
### SYNTHETIC VIDEO INPUTS ###
##############################

## x and y dimension of the frame
dim.x = 64
dim.y = 64

## sample rate in frames per second
fps = 30

## how many seconds does the video last
n.secs = 3 

## what is the frequency at which the boxed region oscillates
sig.f = 2 

## what is the peak amplitude of the signal
sig.peak = 0.5

## size of the boxed region
box.width = 20
box.height = 20


################################
### GENERATE SYNTHETIC VIDEO ###
################################

## use the inputs to generate an image list (i.e. video)
img.list <- gen.eg.img.list(dim.x, dim.y, fps, n.secs, sig.f, sig.peak, box.width, box.height)


#################################
### EXTRACT SIGNAL FROM VIDEO ###
#################################

sig.x <- sig.extract(img.list,fps)

################
### PLOTTING ###
################

## set up a plot
split.screen(c(1,2))
screen(1)
plot(sig.x$org,col='blue',type='l',main='Time Domain')
lines(sig.x$shifted,col='red')

screen(2)
plot(sig.x$ORG,col='blue',type='l',xlim=c(0,5),main='Frequency Domain')
lines(sig.x$SHIFTED,col='red')

## close the screens
close.screen(all.screens=TRUE)


}