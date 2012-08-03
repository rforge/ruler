\name{DigSumSingleRule-class}
\Rdversion{1.1}
\docType{class}
\alias{DigSumSingleRule-class}
\alias{calculateSpecific,DigSumSingleRule,numeric,ANY-method}

\title{Class \code{"DigSumSingleRule"}}
\description{
A class for rules calculating a sum of digits of a single element of numeric sequence.
It inherits from class \code{SingleRule}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DigSumSingleRule", ...)}.

}
\section{Slots}{
  \describe{
    \item{\code{previousRule}:}{Object of class \code{"SingleRule"} rule to be executed on an element of numeric sequence before \code{DigSumSingleRule}, by default is \code{NULL}}.
    \item{\code{.S3Class}:}{Object of class \code{"character"} -  S3 methods apply to this object }
  }
}
\section{Extends}{
Class \code{"\linkS4class{SingleRule}"}, directly.
}
\section{Methods}{
  \describe{
    \item{calculateSpecific}{\code{signature(x = "DigSumSingleRule", y = "numeric", z = "ANY")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{calculate}},
\code{\link{createSR}},
\code{\link{createDR}},
\code{\linkS4class{SingleRule}}
}
\examples{

##[1] simple single rule
b<-new("DigSumSingleRule")
print(b)

b<-createSR(a1=5,n=0) #See 'createSR' for details
print(b)

##[2] combining single rules
b<-new("DigSumSingleRule",previousRule=new("DigSumSingleRule"))#or b<-createSR(a1=5,cv1=0,n=1,5,0)
print(b)

#name: DigSumSingleRule
#name: DigSumSingleRule

calculate(b,785) #2 because 7+8+5=20; 2+0=2

}
\keyword{classes}
