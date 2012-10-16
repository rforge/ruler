\name{DoubleRule-class}
\Rdversion{1.1}
\docType{class}
\alias{DoubleRule-class}

\title{Class \code{"DoubleRule"}}
\description{
A class for rules operating on two elements of a numeric sequence.
It is a virtual class - so no objects of this class are generated. 
All classes for specific rules operating on two arguiments (ex. \code{AddDoubleRule},\code{DivDoubleRule} etc.)
inherit from class \code{DoubleRule}.
}
\section{Objects from the Class}{
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{firstRule}:}{ Object of class \code{"SingleRule"} \code{SingleRule} to be executed on the first element of numeric sequence (before the \code{DoubleRule}). This doesn't need to be specified. }
    \item{\code{secondRule}:}{ Object of class \code{"SingleRule"} \code{SingleRule} to be executed on the second element of numeric sequence (before the \code{DoubleRule}). This doesn't need to be specified. }
    \item{\code{nextSingle}:}{ Object of class \code{"SingleRule"} \code{SingleRule} to be executed on the result of \code{DoubleRule}. This doesn't need to be specified.}
    \item{\code{.S3Class}:}{Object of class \code{"character"} Object of class \code{"character"} S3 methods apply for these objects }
  }
}
\section{Methods}{
  \describe{
    \item{calculate}{\code{signature(x = "DoubleRule", y = "numeric", z = "numeric")}: ... }
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
\code{\link{doubleRules}},
\code{\linkS4class{AddDoubleRule}},
\code{\linkS4class{MultDoubleRule}},
\code{\link{createSR}},
\code{\link{createDR}}

}
\examples{

}
\keyword{classes}
