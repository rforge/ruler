\name{DictionaryRule-class}
\Rdversion{1.1}
\docType{class}
\alias{DictionaryRule-class}

\title{Class \code{"DictionaryRule"}}
\description{
A class of objects used to form a dictionary of rules for a test.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DictionaryRule", ...)} \cr or
\code{createDictRule(index,range=c(-15:-1,1:15))}
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{rule}:}{an object of class \code{Rule}, i.e. either \code{SingleRule} or \code{DoubleRule} }
    \item{\code{range}:}{an object of class \code{vecORnull}, i.e. either a vector or a \code{NULL}. In the former case a range from which constant values can be generated. \cr
    For objects of class \code{DoubleRule} it will be \code{NULL} }
    \item{\code{.S3Class}:}{Object of class \code{character}}
  }
}
\section{Methods}{
No methods defined with class "DictionaryRule" in the signature.
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
\code{\link{createDictRule}},\code{\link{createTest}}, \code{\linkS4class{SingleRule}}, \code{\linkS4class{DoubleRule}}
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{

%%
}
\keyword{classes}
