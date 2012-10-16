\name{IdenSingleRule-class}
\Rdversion{1.1}
\docType{class}
\alias{IdenSingleRule-class}
%\alias{calculateSpecific,IdenSingleRule,numeric,ANY-method}

\title{Class \code{"IdenSingleRule"}}
\description{
A class for rules which do not modify input. This class can be seen as a basic building block. Objects of this class inherit from \code{SingleRule}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("IdenSingleRule", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{previousRule}:}{Object of class \code{"SingleRule"}  -  a rule that will be executed before \code{IdenSingleRule} (this is an argument used to combine different rules together)}
    \item{\code{.S3Class}:}{Object of class \code{"character"} -  S3 methods apply to this object }
  }
}
\section{Extends}{
Class \code{"\linkS4class{SingleRule}"}, directly.
}
\section{Methods}{
  \describe{
    \item{calculateSpecific}{\code{signature(x = "IdenSingleRule", y = "numeric", z = "ANY")}: ... }
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
m<-new("IdenSingleRule")
print(m)
}
\keyword{classes}
