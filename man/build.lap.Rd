\name{build.lap}
\alias{build.lap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Build 5-Point Laplacian Stencil
}
\description{
Given an x and y dimension, build a five point stencil of matching dimensions whose values are either -4, 0, and 1 and whose sum = 0.    
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
build.lap(xdim,ydim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xdim}{
%%     ~~Describe \code{x} here~~
x dimension of desired matrix output
}
\item{ydim}{
y dimension of desired matrix output
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Note that if \code{xdim} or \code{ydim} are even, the stencil values will be off center.  This can be a problem when using a Laplacian matrix for multiple filtering operations.
}
\value{
Matrix with dimensions equal to \code{xdim} and \code{ydim} with five point stencil located near the middle of the matrix (see Details). 
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
\code{\link{build.gaus}}
}
\examples{
## build a 5 point stencil laplacian
lap=build.lap(9,9)
image(lap)
}
