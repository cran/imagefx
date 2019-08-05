\name{crop.image}
\alias{crop.image}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Crop an Image
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Crop an image (matrix or array) with either pre determined bottom-left and top-right locations or interactively.
}
\usage{
crop.image(img, xleft, ybottom, xright, ytop, pick)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{img}{
%%     ~~Describe \code{img} here~~
matrix or array of image to crop
}
  \item{xleft}{
%%     ~~Describe \code{xleft} here~~
left extreme of crop area
}
  \item{ybottom}{
%%     ~~Describe \code{ybottom} here~~
bottom extreme of crop area
}
  \item{xright}{
%%     ~~Describe \code{xright} here~~
right extreme of crop area
}
  \item{ytop}{
%%     ~~Describe \code{ytop} here~~
top extreme of crop area
}
  \item{pick}{
%%     ~~Describe \code{pick} here~~
logical value indicating whether crop region should be selected interactively (see details)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
if any of the \code{xleft}, \code{xright}, \code{ybottom}, \code{ytop} are missing, or if \code{pick} is \code{TRUE}, an interactive plot will aid in picking crop regions.  The original image will be plotted and the user must first select the bottom left corner, then the top right corner of the desired crop area.  A corresponding rectangle will be plotted indicating the current crop region.  If the region is sufficient, the user should then click \emph{crop} in the top right corner of the plotting area.  If the region should be modified, the user should click \emph{repick}.

Note that the \code{xleft}, \code{xright}, \code{ybottom}, and \code{ytop} locations correspond to R's reference frame for matrices, which can be confusing.

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
List of length two with
\item{img.crop}{an object giving the cropped image with the same class (either matrix or array) of \code{img} }
\item{img.corners}{a vector with length 4 giving the the left, right, bottom, and top crop coordinates in the original image. }
}
\author{
%%  ~~who you are~~
Alex J.C. Witsil
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{locator}}
}
\examples{

############
### EG 1 ###
############
## example where you know where to crop the image

sakurajima.crop <- crop.image(sakurajima,xleft=146,ybottom=7,xright=203,ytop=256)
split.screen(c(1,2))
screen(1)
image2(sakurajima,asp=1,main='Original')
screen(2)
image2(sakurajima.crop[[1]],asp=1,main='Cropped')

## close screens
close.screen(all.screens=TRUE)

############
### EG 2 ###
############
## example where you choose where to crop using interactive plot

sakurajima.crop <- crop.image(sakurajima)

split.screen(c(1,2))
screen(1)
image2(sakurajima,asp=1,main='Original')
screen(2)
image2(sakurajima.crop[[1]],asp=1,main='Cropped')
print(sakurajima.crop[[2]])

## close screens
close.screen(all.screens=TRUE)

}
