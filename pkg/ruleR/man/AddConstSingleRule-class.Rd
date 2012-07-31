\name{AddConstSingleRule-class}
\Rdversion{1.1}
\docType{class}
\alias{AddConstSingleRule-class}
\alias{calculateSpecific,AddConstSingleRule,numeric,ANY-method}

\title{Class \code{"AddConstSingleRule"}}
\description{
 A class for rule adding a constant value to the previous element of numeric sequence
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("AddConstSingleRule", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{constantVal}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{previousRule}:}{Object of class \code{"SingleRule"} ~~ }
    \item{\code{.S3Class}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{SingleRule}"}, directly.
}
\section{Methods}{
  \describe{
    \item{calculateSpecific}{\code{signature(x = "AddConstSingleRule", y = "numeric", z = "ANY")}: ... }
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
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("AddConstSingleRule")
}
\keyword{classes}
