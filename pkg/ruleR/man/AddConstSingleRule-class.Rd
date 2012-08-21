\name{AddConstSingleRule-class}
\Rdversion{1.1}
\docType{class}
\alias{AddConstSingleRule-class}
\alias{calculateSpecific,AddConstSingleRule,numeric,ANY-method}

\title{Class \code{"AddConstSingleRule"}}
\description{
 A class for rules which add a specified constant value to the previous element of numeric sequence.
 Objects of this class inherit from \code{SingleRule}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("AddConstSingleRule", constantVal)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{constantVal}:}{Object of class \code{"numeric"} - a constant that will be added to the previous element of numeric sequence }
    \item{\code{previousRule}:}{Object of class \code{"SingleRule"} -  a rule that will be executed before \code{AddConstSingleRule} (this is an argument used to combine different rules together)}
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
Maria Rafalak <m.rafalak@practest.com.pl>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{calculate}}, 
\code{\linkS4class{SingleRule}},
\code{\linkS4class{DoubleRule}},
\code{\link{createSR}},
\code{\link{createDR}}

}
\examples{
##[1] A rule that adds 4 to the previous element of numeric sequence

m <- new("AddConstSingleRule",constantVal=4) #or 
m <- createSR(a1=2,cv1=4,n=0)
print(m)


##[2] combining rules

p <- new("DigSumSingleRule") # a rule that calculates digitsum of a given element
m <- new("AddConstSingleRule",constantVal=5, previousRule=p)

# or in one step:  
# m <- createSR(a1=2, cv1=5, n=1, 5, 0)
print(m)

}
\keyword{classes}
\keyword{AddConstSingleRule}
