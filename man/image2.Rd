\name{image2}
\alias{image2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Plot Images (Matrices) with Intuitive Axes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Plot images (array or matrix) using the \code{rasterImage} function.
}
\usage{
image2(img, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{img}{
%%     ~~Describe \code{img} here~~
numeric matrix or array to plot
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
additional arguments to pass to the \code{plot} function
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Note the difference in image orientation between \code{image()} and \code{image2()}.  These two plots will have the origin in different locations and can make things tricky when picking areas to crop, etc...
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Nothing.  Returns a plotting window.
}
\author{
%%  ~~who you are~~
Alex J.C. Witsil
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{rasterImage}}
}
\examples{
image2(sakurajima)
}
