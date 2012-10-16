\name{SingleRule-class}
\Rdversion{1.1}
\docType{class}
\alias{SingleRule-class}
%\alias{calculate,SingleRule,numeric,ANY-method}
%\alias{calculateSpecific,SingleRule,numeric,ANY-method}

\title{Class \code{"SingleRule"}}
\description{
rules operating on single arguments
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SingleRule", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{previousRule}:}{Object of class \code{"SingleRule"} ~~ }
    \item{\code{.S3Class}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{Rule}"}, directly.
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
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("SingleRule")
}
\keyword{classes}
\keyword{internal}
