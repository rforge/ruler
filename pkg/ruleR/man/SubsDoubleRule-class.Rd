\name{SubsDoubleRule-class}
\Rdversion{1.1}
\docType{class}
\alias{SubsDoubleRule-class}
\alias{calculateSpecific,SubsDoubleRule,numeric,numeric-method}

\title{Class \code{"SubsDoubleRule"}}
\description{
A class for rules substracting the second element of numeric sequence from the first one. 
Objects of this class inherit from class \code{DoubleRule}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SubsDoubleRule", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{firstRule}:}{Object of class \code{"SingleRule"} \code{SingleRule} to be executed on the first element of numeric sequence (before the \code{DoubleRule}). This doesn't need to be specyfied.}
    \item{\code{secondRule}:}{Object of class \code{"SingleRule"} \code{SingleRule} to be executed on the second element of numeric sequence (before the \code{DoubleRule}). This doesn't need to be specyfied. }
    \item{\code{nextSingle}:}{Object of class \code{"SingleRule"}  \code{SingleRule} to be executed on the result of \code{DoubleRule}. This doesn't need to be specyfied. }
    \item{\code{.S3Class}:}{Object of class \code{"character"} S3 methods apply for these objects}
  }
}
\section{Extends}{
Class \code{"\linkS4class{DoubleRule}"}, directly.
}
\section{Methods}{
  \describe{
    \item{calculateSpecific}{\code{signature(x = "SubsDoubleRule", y = "numeric", z = "numeric")}: ... }
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
\code{\linkS4class{DoubleRule}},
\code{\linkS4class{SingleRule}},
\code{\link{createSR}},
\code{\link{createDR}}
}
\examples{

m<-new("SubsDoubleRule") 
print(m)

calculate(m,20,7) # 20-7=13

}
\keyword{classes}
