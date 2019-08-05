\name{erebus}
\alias{erebus}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Image of Erebus Volcano, Antarctica
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
JPEG image read in as an array where the third dimension corresponds to the red, green, and blue color channels.  Note the image has been compressed significantly to reduce the memory size.
}
\usage{data("erebus")}
\format{
  The format is:
 num [1:300, 1:400, 1:3] 0.019768 0.0011 0.002516 0 0.000495 ...
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Witsil and Johnson (2018) <10.1016/j.jvolgeores.2018.05.002>
}
\examples{
data(erebus)
image2(erebus,asp=1)
## maybe str(erebus) ; plot(erebus) ...
}
\keyword{datasets}
