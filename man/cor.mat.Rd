\name{cor.mat}
\alias{cor.mat}
\title{
Correlate Matrix Rows
}
\description{
Wrapper function of ccf used to find best lag time between two vectors. 
}
\usage{
cor.mat(x, y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Vector. 
}
  \item{y}{
    Vector. 
}
  \item{\dots}{
    Additional arguments to pass to ccf (e.g. \code{max.lag})
}
}

\value{
Scalar indicating the lag associated with the maximum correlation value between \code{x} and \code{y}.  
}
\author{
Alex J.C. Witsil
}

\seealso{
\code{\link{ccf}}
}
\examples{
## generate a time axis
tax = seq(0,10,by=0.1)

## generate two signals with a phase offset
sig1 <- sin(2*pi*1/2*tax)
sig2 <- sin(2*pi*1/2*tax + pi/2)

best.lag <- cor.mat(sig1,sig2)

################
### PLOTTING ###
################

plot(sig1,type='l',col='blue',main=paste('lag is: ',best.lag,sep=''))
lines(sig2,col='green')

}

