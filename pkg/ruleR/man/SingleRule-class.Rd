\name{SingleRule-class}
\Rdversion{1.1}
\docType{class}
\alias{SingleRule-class}
\alias{calculate,SingleRule,numeric,ANY-method}
\alias{calculateSpecific,SingleRule,numeric,ANY-method}

\title{Class \code{"SingleRule"}}
\description{
A class for rules operating on one element of numeric sequence.
It is a virtual class - so no objects of this class are generated. 
All classes for specyfic rules operating on one arguiment (ex. \code{AddConstRule}, \code{MultConstSingleRule} etc.)
inherit from class \code{SingleRule}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SingleRule", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{previousRule}:}{Object of class \code{"SingleRule"} -  a rule that will be executed before the main SingleRule }
    \item{\code{.S3Class}:}{Object of class \code{"character"} -  S3 methods apply to this object }
  }
}
\section{Methods}{
  \describe{
    \item{calculate}{\code{signature(x = "SingleRule", y = "numeric", z = "ANY")}: ... }
    \item{calculateSpecific}{\code{signature(x = "SingleRule", y = "numeric", z = "ANY")}: ... }
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
\code{\link{singleRules}},
\code{\linkS4class{IdenSingleRule}},
\code{\linkS4class{AddConstSingleRule}},
\code{\linkS4class{MultConstSingleRule}},
\code{\linkS4class{DigSumSingleRule}},
\code{\linkS4class{NegativeSingleRule}},
\code{\link{createSR}},
\code{\link{createDR}},
\code{\linkS4class{DoubleRule}}
}
\examples{

}
\keyword{classes}
