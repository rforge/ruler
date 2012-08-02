\name{NegativeSingleRule-class}
\Rdversion{1.1}
\docType{class}
\alias{NegativeSingleRule-class}
\alias{calculateSpecific,NegativeSingleRule,numeric,ANY-method}

\title{Class \code{"NegativeSingleRule"}}
\description{
A class for rules which create a negative value from single element of numeric sequence.
 Objects of this class inherit from \code{SingleRule}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("NegativeSingleRule", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{previousRule}:}{Object of class \code{"SingleRule"} -  a rule that will be executed before \code{NegativeSingleRule} (this is an argument used to combine different rules together)}
    \item{\code{.S3Class}:}{Object of class \code{"character"} -  S3 methods apply to this object }
  }
}
\section{Extends}{
Class \code{"\linkS4class{SingleRule}"}, directly.
}
\section{Methods}{
  \describe{
    \item{calculateSpecific}{\code{signature(x = "NegativeSingleRule", y = "numeric", z = "ANY")}: ... }
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
\code{\linkS4class{SingleRule}},
\code{\linkS4class{DoubleRule}}

}
\examples{
m<-new("NegativeSingleRule") #or m<-createSR(a1=6,n=0)
print(m)

calculate(m,4)#-4

}
\keyword{classes}
