\name{SubsConstSingleRule-class}
\Rdversion{1.1}
\docType{class}
\alias{SubsConstSingleRule-class}
\alias{calculateSpecific,SubsConstSingleRule,numeric,ANY-method}

\title{Class \code{"SubsConstSingleRule"}}
\description{
 A class for rules which substracts a specyfied constant value from the previous element of numeric sequence.
 Objects of this class inherit from \code{SingleRule}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SubsConstSingleRule", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{constantVal}:}{Object of class \code{"numeric"} - a constant that will be added to the previous element of numeric sequence }
    \item{\code{previousRule}:}{Object of class \code{"SingleRule"} -  a rule that will be executed before \code{SubsConstSingleRule} (this is an argument used to combine different rules together) }
    \item{\code{.S3Class}:}{Object of class \code{"character"} -  S3 methods apply to this object }
  }
}
\section{Extends}{
Class \code{"\linkS4class{SingleRule}"}, directly.
}
\section{Methods}{
  \describe{
    \item{calculateSpecific}{\code{signature(x = "SubsConstSingleRule", y = "numeric", z = "ANY")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{


}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{calculate}}, 
\code{\linkS4class{SingleRule}},
\code{\linkS4class{DoubleRule}},
\code{\link{createSR}},
\code{\link{createDR}}
}
\examples{
m<-new("SubsConstSingleRule",constantVal=41) #or m<-createSR(a1=4,cv1=41,n=0)
print(m)
calculate(m,34)#34-41=-7
}
\keyword{classes}
