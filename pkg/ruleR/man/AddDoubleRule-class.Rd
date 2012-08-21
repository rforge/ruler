\name{AddDoubleRule-class}
\Rdversion{1.1}
\docType{class}
\alias{AddDoubleRule-class}
\alias{calculateSpecific,AddDoubleRule,numeric,numeric-method}

\title{Class \code{"AddDoubleRule"}}
\description{
 A class for the rule which adds two previous elements of a number sequence.
 It inherits from class \code{DoubleRule}
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("AddDoubleRule",... )}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{firstRule}:}{Object of class \code{"SingleRule"} - a rule to be executed on the first element of numeric sequence, before executing DoubleRule (doesn't need to be specified)}
    \item{\code{secondRule}:}{Object of class \code{"SingleRule"} - a rule to be executed on the second element of numeric sequence, before executing DoubleRule (doesn't need to be specified) }
    \item{\code{nextSingle}:}{Object of class \code{"SingleRule"} - a rule to be executed on the result of DoubleRule (doesn't need to be specified) }
    \item{\code{.S3Class}:}{Object of class \code{"character"}  - S3 methods apply for these objects }
  }
}
\section{Extends}{
Class \code{"\linkS4class{DoubleRule}"}, directly.
}
\section{Methods}{
  \describe{
    \item{calculateSpecific}{\code{signature(x = "AddDoubleRule", y = "numeric", z = "numeric")}: ... }
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
\code{\linkS4class{DoubleRule}},
\code{\linkS4class{SingleRule}},
\code{\link{createSR}},
\code{\link{createDR}}



}
\examples{

#[1] simple DoubleRule

p <- new("AddDoubleRule")
calculate(p,12,5) #12+5=17



#[2] combining rules

m <- new("DigSumSingleRule") 
# SingleRule that calculates digitsum of an element of numeric sequence

r <- new("AddConstSingleRule", constantVal=6)
# a SingleRule that adds constant value '6' to the element of numeric sequence 

f <- new("MultConstSingleRule", constantVal=2)
# a SingleRule that muliplies an element of a sequence by constant value '4'

g <- new("AddDoubleRule", firstRule=r, secondRule=f, nextSingle=m)
calculate(g,12,5) #10

## How is rule 'g' executed ?
## Starting elements are '12' and '5'
## Rule 'r'(firstRule) is applied to the first element of numeric sequence (12+6=18)
## Rule 'f'(secondRule) is applied to the second element of numeric sequence (5*2=10)
## "AddDoubleRule" is executed on the results of firstRule and secondRule (18+10=28)
## Rule 'm' is applies to the result of DoubleRule (2+8=10)

}
\keyword{classes}
\keyword{AddDoubleRule}
