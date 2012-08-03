\name{AddRule}
\alias{AddRule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to add rules specyfied by user to a list
}
\description{

When creating test items test rules can be either combined automatically or
be generated from the set of rules defined by user (stored on \code{MyRule} list).
\code{AddRule} is a function to add a rule defined by a user to \code{MyRule} list.

}
\usage{
AddRule(rule)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rule}{
 argument \code{rule} must inherit either form class \code{"SingleRule"} or \code{"DoubleRule"}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{automaticTest}},
\code{\link{MyRules}},
\code{\link{createSR}},
\code{\link{createDR}}

}
\examples{

# 'MyRules' list by default consists only of one element, which is NA.


# creating sample rules (this should be done by user)
n<-new("SubsDoubleRule")
p<-new("AddConstSingleRule", constantVal=6)

AddRule(n) #"Your rule succesfully added tp 'MyRule' list"
AddRule(p) #"Your rule succesfully added tp 'MyRule' list"
AddRule(n)# Error in AddRule(n) : There already is a rule that gives the same result.

}

\keyword{AddRule}
