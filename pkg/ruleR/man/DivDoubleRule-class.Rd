\name{DivDoubleRule-class}
\Rdversion{1.1}
\docType{class}
\alias{DivDoubleRule-class}
\alias{calculateSpecific,DivDoubleRule,numeric,numeric-method}

\title{Class \code{"DivDoubleRule"}}
\description{
A class for rules deviding first element of numeric sequence by the second one. 
It inherits from class \code{DoubleRule}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DivDoubleRule", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{firstRule}:}{Object of class \code{"SingleRule"} - \code{SingleRule} to be executed on the first element of numeric sequence (before the \code{DoubleRule}). This doesn't need to be specyfied. }
    \item{\code{secondRule}:}{Object of class \code{"SingleRule"} -  \code{SingleRule} to be executed on the second element of numeric sequence (before the \code{DoubleRule}). This doesn't need to be specyfied. }
    \item{\code{nextSingle}:}{Object of class \code{"SingleRule"} -  \code{SingleRule} to be executed on the result of \code{DoubleRule}. This doesn't need to be specyfied.}
    \item{\code{.S3Class}:}{Object of class \code{"character"} -  S3 methods apply for these objects }
  }
}
\section{Extends}{
Class \code{"\linkS4class{DoubleRule}"}, directly.
}
\section{Methods}{
  \describe{
    \item{calculateSpecific}{\code{signature(x = "DivDoubleRule", y = "numeric", z = "numeric")}: ... }
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

m<-new("DivDoubleRule") 
print(m)

calculate(m,20,10) #20/10=2

}
\keyword{classes}
