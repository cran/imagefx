\name{erebus.90}
\alias{erebus.90}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Image of Erebus Volcano, Antarctica
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
The 90th frame in a series of images that was recorded seconds after a bubble bursting event at Mount Erebus.

JPEG image read in as an array where the third dimension corresponds to the red, green, and blue color channels. Note the image has been compressed significantly to reduce the memory size.
}
\usage{data("erebus.90")}
\format{
  The format is:
 num [1:225, 1:300, 1:3] 0.03922 0.00392 0.01176 0 0.01569 ...
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Witsil and Johnson (2018) <10.1016/j.jvolgeores.2018.05.002>
}
\examples{
data(erebus.90)
image2(erebus.90)
}
\keyword{datasets}
