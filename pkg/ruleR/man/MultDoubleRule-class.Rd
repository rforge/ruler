\name{MultDoubleRule-class}
\Rdversion{1.1}
\docType{class}
\alias{MultDoubleRule-class}
%\alias{calculateSpecific,MultDoubleRule,numeric,numeric-method}

\title{Class \code{"MultDoubleRule"}}
\description{
A class for rules multiplying two elements of a numeric sequence. 
Objects of this class inherit from class \code{DoubleRule}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MultDoubleRule", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{firstRule}:}{Object of class \code{"SingleRule"} - \code{SingleRule} to be executed on the first element of numeric sequence (before the \code{DoubleRule}).This doesn't need to be specyfied.}
    \item{\code{secondRule}:}{Object of class \code{"SingleRule"} -  \code{SingleRule} to be executed on the second element of numeric sequence (before the \code{DoubleRule}).This doesn't need to be specyfied. }
    \item{\code{nextSingle}:}{Object of class \code{"SingleRule"} -  \code{SingleRule} to be executed on the result of \code{DoubleRule}. This doesn't need to be specyfied.}
    \item{\code{.S3Class}:}{Object of class \code{"character"} -  S3 methods apply for these objects }
  }
}
\section{Extends}{
Class \code{"\linkS4class{DoubleRule}"}, directly.
}
\section{Methods}{
  \describe{
    \item{calculateSpecific}{\code{signature(x = "MultDoubleRule", y = "numeric", z = "numeric")}: ... }
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

m<-new("MultDoubleRule") 
print(m)

calculate(m,20,9) #20*9=180

}
\keyword{classes}
