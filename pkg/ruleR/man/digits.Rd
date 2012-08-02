\name{digits}
\alias{digits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
That is a function to separate digits from a number. 
This function is used in \code{DigSumSingleRule}, which calculates the sum of digits.
}
\usage{
digits(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric value of which digits are separated
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
https://stat.ethz.ch/pipermail/r-help/2011-March/270786.html
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{DigSumSingleRule}}
}
\examples{
m<-digits(46)
m[1]#4
m[2]#6
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
