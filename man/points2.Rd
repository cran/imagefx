\name{points2}
\alias{points2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot points on image 
}
\description{
This is a plotting function that should be used with \code{image2}.  This function is in line with \code{image2}, which locates the origin to the top-left corner as opposed to the bottom-left as \code{image} does.  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
points2(x,y,img,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
x location of points to plot
}
\item{y}{
y location of points to plot
}
\item{img}{
original image (matrix) which was plotted using \code{image2}.  This is supplied simply for dimensional information. 
}
\item{...}{
additional arguments to be passed to \code{points}.
}
}
\value{
Nothing.  This is a plotting function.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Alex J.C. Witsil
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{image2}}
}
\examples{
## build a test matrix
mat = matrix(0,nrow=20,ncol=20)
mat[1,1]=1

image2(mat,axes=FALSE,xlab='',ylab='')
points2(1,1,img=mat,col='red',pch=16)
}
