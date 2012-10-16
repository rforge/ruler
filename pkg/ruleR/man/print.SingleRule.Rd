\name{print.SingleRule}
\alias{print.SingleRule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
print.SingleRule
}
\description{
Print objects of class \code{SingleRule}
}
\usage{
print.SingleRule(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class \code{SingleRule}
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
\code{\link{createSR}},
\code{\link{createDR}},
\code{\linkS4class{SingleRule}},
\code{\linkS4class{DoubleRule}}
}
\examples{

b<-createSR(a1=2,cv1=0,n=2,2,14,3,6)
print(b)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
