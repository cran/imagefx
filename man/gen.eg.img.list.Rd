\name{gen.eg.img.list}
\alias{gen.eg.img.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Generate Example Image List Data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Generates synthetic video data comprising noise around each frame's peripheral and a boxed region whose pixel values oscillate according to the input parameters.   
}
\usage{
gen.eg.img.list(dim.x, dim.y, fps, n.secs, sig.f, sig.peak, box.width, box.height)
}

\arguments{
  \item{dim.x}{
    x dimension of the video frames.  Defaults to 64 pixels.
}
\item{dim.y}{ 
    y dimension of the video frames.  Defaults to 64 pixels.
}
\item{fps}{ 
    Sampling rate of the synthetic video in frames per second.  Defaults to 30 frames per second.
}
\item{n.secs}{ 
    Number of seconds in the synthetic video.  Defaults to 3 s.
}
\item{sig.f}{ 
    Frequency at which the boxed region's pixel values oscillate.  Defaults to 2 Hz.
}
\item{sig.peak}{ 
    Peak of signal.  Defaults to 0.2.
}
\item{box.width}{ 
    Width of box region.  Defaults to 10 pixels.
}
\item{box.height}{ 
    Height of box region.  Defaults to 10 pixels.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Use this function to create synthetic video data in the list structure required for functions like \code{\link{amp.sig}} and \code{\link{sig.extract}}.  

Note that noise in the frames fluctuates between -1 and 1.  Therefore, if you choose \code{sig.peak<1} in each frame the boxed region's pixel values will be below the signal to noise ratio.  If you were to 'play' this video (i.e. cycle through the list elements) the boxed region would be difficult or impossible to distinguish without first amplifying the signal using \code{\link{amp.sig}}.
}
\value{
List whose elements correspond to individual video frames, each of which are separated by \code{1/fps}.
}
\author{
Alex J.C. Witsil
}
\seealso{
\code{\link{amp.sig}} and \code{\link{sig.extract}}
}
\examples{

###############
### INPUTS ####
###############

## x and y dimension of the frame
dim.x = 64
dim.y = 64

## sample rate in frames per second
fps = 30

## how many seconds does the video last
n.secs =3 

## what is the frequency at which the boxed region oscillates
sig.f = 2 

## what is the peak amplitude of the signal
sig.peak = 0.2 

## size of the boxed region
box.width = 10
box.height = 10

################################  
### GENERATE SYNTHETIC VIDEO ###
################################

## use the inputs to generate an image list (i.e. video)
img.list <- gen.eg.img.list(dim.x, dim.y, fps, n.secs, sig.f, sig.peak, box.width, box.height)

## or use the defaults in the function
img.list <- gen.eg.img.list()

}