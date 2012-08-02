\name{sequence}
\alias{sequence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
A function to generate numeric sequence of specyfied lenght.
Is used in \code{automaticTest} but can be used separately.
Note that this function doesn't check for constant sequence or
too big/small elements in numeric sequence!
}
\usage{
sequence(x1, x2 = NULL, rule, n = 6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
first element of numeric sequence
}
  \item{x2}{
second element of numeric sequence (if applied rule doesn't need second element, it will be overwritten)
}
  \item{rule}{
a rule you want to apply. It needs to inherit either from class \code{SingleRule} or \code{DoubleRule}
}
  \item{n}{
length of numeric sequence, by default \code{n=6}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
function returns a list containing of:

\item{sequence[[1]] }{numeric sequence as a list}
\item{sequence[[2]]}{rule used to generate the sequence}

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
\code{\link{automaticTest}},
\code{\link{conCheck}},
\code{\link{check}},
\code{\link{createSR}},
\code{\link{createDR}}

}
\examples{
b<-createSR(a1=5,cv1=0,n=1,2,14) 
# creating a rule I want to use in generating numeric sequence

m<-sequence(x1=1,x2=3,rule=b,n=10)

unlist(m[1]) #1  6  2  7  3  8  4  9  5 10
print(m[2])

#name: DigSumSingleRule
#name: AddConstSingleRule, constant value:  14


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
