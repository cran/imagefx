\name{shift.vec}
\alias{shift.vec}
\title{
Shift Vector
}
\description{
Shift a vector to the left or right by a certain amount (i.e. perform a simple phase shift).  
}
\usage{
shift.vec(shift, vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shift}{
    Amount of samples to shift vector in either the positive or negative direction. 
}
  \item{vec}{
    Vector to shift. 
}
}
\details{
  The shift is accomplished by padding the head or tail of the vector with \code{NA} values. 
}
\value{
Shifted vector.  
}
\author{
Alex J.C. Witsil
}
\examples{

## generate a delta function
vec=rep(0,5)
vec[3] = 1 

## shift vector by -2
new.vec = shift.vec(-2,vec)

}
