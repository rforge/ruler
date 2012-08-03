\name{createSR}
\alias{createSR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
functions to create objects of class \code{SingleRule}
}
\description{
function to create objects of class \code{SingleRule}
}
\usage{
createSR(a1 = NULL, cv1 = NULL, n = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a1}{
an index of \code{SingleRule} stored on  \code{singleRules} list to be used while creating new object. If not specyfied it will be generated automatically.
}
  \item{cv1}{
a constant value to be used with \code{singleRules[[a1]]} rule. If not specyfied it will be generated automatically (from range -100:100). 
}
  \item{n}{
argument defining how many rules are to be nested. If \code{n=0} a simple \code{SingleRule} object is created (with no nested rules).
If not specyfied it will be generated automatically together with the parameters for all the nested functions).
\code{n} is generated from values \code{(0,1,2)} but its value is unrestricted when user wants to set that parameter manually.
As combining many rule makes it difficult to solve it, n=0 is generated with greater possibility that other options (\code{1,2}).
}
  \item{\dots}{
parameters for functions to be nested as \code{previousRules}. For every nested rule, user MUST supply TWO arguments, always in the same order:
first an index of table \code{singleRules} in which desired rule is stored, and afterwards a constant value (even if the rule doesn't require one).If the rule doesn't require a constant value, this argument is going to be ignored. See example [1].
You can specify either all parameters for your nested functions or none (so they will be generated automatically).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A function returns an object that inherits from class \code{SingleRule}. 
Returned object will be of class corresponding to applied \code{SingleRule}.
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
\code{\linkS4class{SingleRule}},
\code{\linkS4class{DoubleRule}},
\code{\link{createDR}},
\code{\link{calculate}}

}
\examples{

##[1] Manually defining nested rules

m<-createSR(a1=2,cv1=2,n=3,3,3,4,4,5,5)
print(m)

#name: AddConstSingleRule, constant value:  2 # this rule will be executed in the end
#name: MultConstSingleRule, constant value:  3
#name: SubsConstSingleRule, constant value:  4 # this rule will be executed at second
#name: DigSumSingleRule # this rule will be executed at first (as it is the last nested rule)

# parameters for nested functions are 3,3,4,4,5,5
#it means that I want to nest function singleRules[[3]], which is "MultConstSingleRule"
# with a constant value 3.
# Next I want to nest function singleRules[[4]], which is "SubsConstSingleRule" with a
# constant value 4.
# Finally I want to nest function singleRules[[5]], which is "DigSumSingleRule". As this
# function doesn't need constant value, the last argument '5' is ignored.

calculate(m,14) #5 because ((1+4)-4)*3+2=5


##[2] Defining nested rules with no parameters supplied
g<-createSR(a1=3,cv1=47,n=2)
#parameters for nested rules are generated automatically 
print(g)

#name: MultConstSingleRule, constant value:  47
#name: SubsConstSingleRule, constant value:  5 # generated automatically - will be different in every generation
#name: SubsConstSingleRule, constant value:  3 # generated automatically - will be different in every generation

##[3] Automatically generating object of class SingleRule
r<-createSR()
print(r)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
