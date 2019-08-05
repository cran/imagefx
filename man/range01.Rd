\name{range01}
\alias{range01}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Scale Object Values Between Zero and One
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Force the minimum value and maximum value of an object to 0 and 1, respectively.
}
\usage{
range01(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
Matrix, vector, or other R object.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
An object with same dimensions as \code{x} and whose values range between zero and one.
}
\author{
%%  ~~who you are~~
Alex J.C. Witsil
}
\examples{
## generate a signal to normalize
sig <- 10*sin(2*pi*5*seq(0,1,by=0.001))

## normalize between 0 and 1
sig01 <- range01(sig)

## check the ranges
range(sig)
##[1] -10 10
range(sig01)
##[1] 0 1
}
