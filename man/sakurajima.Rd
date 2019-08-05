\name{sakurajima}
\alias{sakurajima}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Image of Sakurajima Volcano, Japan
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
JPEG image read in as an array where the third dimension corresponds to the red, green, and blue color channels.  Note the image has been compressed significantly to reduce the memory size.
}
\usage{data("sakurajima")}
\format{
  The format is:
 num [1:300, 1:400, 1:3] 0.471 0.475 0.475 0.478 0.475 ...
}
\examples{
data(sakurajima)
image2(sakurajima,asp=1)

}
\keyword{datasets}
