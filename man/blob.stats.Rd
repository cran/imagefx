\name{blob.stats}
\alias{blob.stats}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Blob Statistics from Erebus Volcano, Antarctica
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
Raw statistics output from \code{calc.blob.stats} that show a single bubble bursting event from the lava lake at Mount Erebus.  Each list component corresponds to statistics calculated from the red, green, and blue color channels from the image frames.
}
\usage{data("blob.stats")}
\format{
  The format is:
List of 3
 $ r: num [1:200, 1:14] 331 332 330 334 330 ...
 $ g: num [1:200, 1:14] 567.15 568.84 -3.13 -3.08 -4.33 ...
 $ b: num [1:200, 1:14] 454.8 473.3 461.6 476.2 -2.5 ...
}
\examples{
data(blob.stats)
## maybe str(blob.stats) ; plot(blob.stats) ...
}
\keyword{datasets}
