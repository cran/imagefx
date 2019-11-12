\name{amp.sig}
\alias{amp.sig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Amplify Time Varying Signals in Video
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Filters then amplifies each pixel's time series signal.    
}
\usage{
amp.sig(img.list, fps, f.corners, amp ,n.poles, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{img.list}{
  A series of images saved in a list.  Each list element is a matrix that represents pixel values of an image, the dimensions of which correspond to the rows and columns of the matrix.
  }
  \item{fps}{
  Sample rate of video in frames per second (FPS).  Each list element of \code{img.list} should be separated by \code{1/fps} seconds. Defaults to 30. 
  }
  \item{f.corners}{
  Corners to use in the filter.  Currently only a Butterworth filter is implemented.
  }
  \item{amp}{
  Scalar to multiply filtered signals by.  Defaults to 10. 
  }
  \item{n.poles}{
  Number of poles used in Butterworth filter.  Defaults to two. 
  }
  \item{type}{
  Type of filter used in Butterworth filter (i.e. 'low', 'high', 'stop', 'pass'.  Defaults to 'pass' if \code{n.poles==2} and 'high' if \code{n.poles==1}.  
  }
} 
\details{
This algorithm is based on the landmark study by Wu et. al (2012) though is organized and implemented slightly differently.  Namely, this algorithm takes advantage of R functionality instead of the original development language, MATLAB.     

If the goal is to amplify subtle time variations in an image (e.g. heartbeat, breathing, volcanic emissions, etc...) choose corner frequencies that contain the desired signal.  For example, most adult resting heart rates range between 1 and 1.5 Hz.  To amplify these signals, use a filter type of either \code{low}, \code{high}, or \code{pass}.  If the goal is to dampen or remove signals, choose appropriate corner frequencies and use a filter type of \code{low}, \code{high}, or \code{stop}.  To help choose appropriate corner frequencies, see \code{\link{sig.extract}}.  

Filtering is accomplished via the \code{apply} function for speed.  However, if the dimensions of the images are large (i.e. \code{dim(img.list[[1]])}) or if the number of frames is large (i.e. \code{length(img.list)}), the RAM may fill up and the R session may crash. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
List with the same length and element dimensions as \code{img.list}.  Each list element contains the amplified version of the original image list.   
}
\references{
Wu, Hao-Yu, et al. "Eulerian video magnification for revealing subtle changes in the world." (2012) 
}
\author{
Alex J.C. Witsil
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{sig.extract}}
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
n.secs =3 

## what is the frequency at which the boxed region oscillates
sig.f = 2 

## what is the peak amplitude of the signal
sig.peak = 0.2 

## size of the boxed region
box.width = 10
box.height = 10


##################################
### VIDEO AMPLIFICATION INPUTS ###
##################################

## how much to amplify the signal by
amp=500

## filter corners -- should contain the signal of interest
f.corners = c(sig.f-0.2, sig.f+0.2)

## number of poles in Butterworth filter
n.poles = 2

## type of filter
type = 'pass'


################################
### GENERATE SYNTHETIC VIDEO ###
################################

## use the inputs to generate an image list (i.e. video)
img.list <- gen.eg.img.list(dim.x, dim.y, fps, n.secs, sig.f, sig.peak, box.width, box.height)


################################
### AMPLIFY THE VIDEO SIGNAL ###
################################

## amplify the video signal
amp.list <- amp.sig(img.list, fps, f.corners, amp, n.poles, type)


################
### PLOTTING ###
################

## save the users original margins
mar.org  <- par()$mar

## generate a time axis
tax <- (1:(n.secs*fps)) / fps 

## get the raw video into a spatiotemporal matrix
org.spat.temp <- matrix(unlist(lapply(img.list,rowSums)),ncol=length(tax))
amp.spat.temp <- matrix(unlist(lapply(amp.list,rowSums)),ncol=length(tax))

## define some example frames to plot
eg.frames = c(18,26,34,41)

## define a layout matrix 
layout.mat <- matrix(c(1:length(eg.frames), rep(5,4),6:9,rep(10,4)),nrow=2,byrow=TRUE)
layout(layout.mat)

## plot some example 'frames' from the original video
i=1
while(i<=length(eg.frames)) {
    par(mar=c(2,1,3,1))
    
    ## make the current title
    c.main = paste('Org. Frame ', eg.frames[i], sep='')
    image2(img.list[[eg.frames[i]]],axes=FALSE,main=c.main,cex.main=2,asp=1)
      
    i=i+1
}

## plot the spatiotemporal variations of the original and amplified video
par(mar=c(2,1,3,1))
image2(org.spat.temp, ylab='y',axes=FALSE,xaxs='i',asp=1)
mtext('y', side=2,line=0.5)
axis(1)
box()

## plot the same example frames from the amplified video
i=1
while(i<=length(eg.frames)) {
    par(mar=c(2,1,3,1))
    ## make the current title
    c.main = paste('Amp. Frame ', eg.frames[i], sep='')
    
    ## add the image
    image2(amp.list[[eg.frames[i]]],axes=FALSE,main=c.main,cex.main=2,asp=1)
      
    i=i+1
}

par(mar=c(2,1,3,1))
image2(amp.spat.temp, xlab='',ylab='',axes=FALSE,xaxs='i',asp=1)
axis(3)
mtext('y', side=2,line=0.5)
mtext('frames', side=1,line=0.5)
box()


## set the layout and par back to the users original value
par(mfrow=c(1,1),mar=mar.org)

}
