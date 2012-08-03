\name{duplicate}
\alias{duplicate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function
}
\description{
A function checking whether a specyfied vector is already in a specyfied matrix. 
This function is called by \code{automaticTest} while generating numeric sequences for the test, or
by \code{AddRule}.
}
\usage{
duplicate(mx, vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mx}{
a matrix in which function \code{duplicate} searches for a specyfied vector
}
  \item{vec}{
a vector for which a specyfied matrix is searched 
}
}
\details{
Length of the vector must be equal to the number of columns in the searched matrix.
}
\value{
This function can return two values
\item{TRUE}{means that such vector already is in the matrix}
\item{FALSE}{means that no such vector is in the matrix}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{automaticTest}}
}
\examples{

m<-matrix(c(1:5,5:1,c(1,2,5,4,3)),3,5,byrow=TRUE)
r<-1:5
g<-c(1:4,4)

duplicate(mx=m,vec=r) # TRUE - there is already such vector in a matrix
duplicate(mx=m,vec=g) #FALSE - no cuch vector in the matrix

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
