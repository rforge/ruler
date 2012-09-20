\name{MultConstSingleRule-class}
\Rdversion{1.1}
\docType{class}
\alias{MultConstSingleRule-class}
%\alias{calculateSpecific,MultConstSingleRule,numeric,ANY-method}

\title{Class \code{"MultConstSingleRule"}}
\description{
A class for rules which multiply previous element of numeric sequence by a specyfied constant.
 Objects of this class inherit from \code{SingleRule}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MultConstSingleRule", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{constantVal}:}{Object of class \code{"numeric"} - a constant that will be added to the previous element of numeric sequence }
    \item{\code{previousRule}:}{Object of class \code{"SingleRule"} -  a rule that will be executed before \code{MultConstSingleRule} (this is an argument used to combine different rules together) }
    \item{\code{.S3Class}:}{Object of class \code{"character"} -  S3 methods apply to this object }
  }
}
\section{Extends}{
Class \code{"\linkS4class{SingleRule}"}, directly.
}
\section{Methods}{
  \describe{
    \item{calculateSpecific}{\code{signature(x = "MultConstSingleRule", y = "numeric", z = "ANY")}: ... }
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
\code{\linkS4class{SingleRule}},
\code{\linkS4class{DoubleRule}},
\code{\link{createSR}},
\code{\link{createDR}}
}
\examples{
m<-new("MultConstSingleRule",constantVal=4,previousRule=new("IdenSingleRule")) #or m<-createSR(a1=3,cv1=4,n=0)
print(m)

calculate(m,50)#50*4=200

}
\keyword{classes}
