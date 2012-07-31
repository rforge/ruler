\name{DoubleRule-class}
\Rdversion{1.1}
\docType{class}
\alias{DoubleRule-class}
\alias{calculate,DoubleRule,numeric,numeric-method}

\title{Class \code{"DoubleRule"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DoubleRule", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{firstRule}:}{Object of class \code{"SingleRule"} ~~ }
    \item{\code{secondRule}:}{Object of class \code{"SingleRule"} ~~ }
    \item{\code{nextSingle}:}{Object of class \code{"SingleRule"} ~~ }
    \item{\code{.S3Class}:}{Object of class \code{"character"} ~~ }
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
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("DoubleRule")
}
\keyword{classes}
