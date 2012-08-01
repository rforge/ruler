\name{AddConstSingleRule-class}
\Rdversion{1.1}
\docType{class}
\alias{AddConstSingleRule-class}
\alias{calculateSpecific,AddConstSingleRule,numeric,ANY-method}

\title{Class \code{"AddConstSingleRule"}}
\description{
 A class for rules which add a constant value to the previous element of numeric sequence.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("AddConstSingleRule", constantVal)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{constantVal}:}{Object of class \code{"numeric"} - a constant that will be added to the previous element of numeric sequence }
    \item{\code{previousRule}:}{Object of class \code{"SingleRule"} -  a rule that will be executed before AddConstSingleRule (this is an argument used to combine different rules together)}
    \item{\code{.S3Class}:}{Object of class \code{"character"} -  S3 methods apply to this object }
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
\code{\link{calculate}}, a functions to execute the rule
\code{\linkS4class{SingleRule}}
}
\examples{
##[1]A rule that adds 4 to the previous element of numeric sequence

m<-new("AddConstSingleRule",constantVal=4) 
calculate(m,16) #16+4=20




##[2]combining rules

p<-new("DigSumSingleRule") 
# a rule that calculates digitsum of a given element

m<-new("AddConstSingleRule",constantVal=5, previousRule=p)

calculate(m,40) #9

## How is rule 'g' executed ?
## rule 'p' (previousRule) is executed on element 40 (4+0=4)
## rule 'm' is executed on the result of previousRule (4+5=9)

## You can combine many different rules i.e. rule 'm' can be a 
## previousRule argument of another rule etc.



}
\keyword{classes}
\keyword{AddConstSingleRule}
