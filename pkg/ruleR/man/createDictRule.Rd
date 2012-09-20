\name{createDictRule}
\alias{createDictRule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createDictRule
}
\description{
A function to create an object of class DictionaryRule
}
\usage{
createDictRule(index, range = c(-15:-1, 1:15))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{index}{
An index of DoubleRule or SingleRule in a table of rules.
\cr 1 - IdenSingleRule,
\cr 2 - AddConstSingleRule,
\cr 3 - MultConstSingleRule,
\cr 4 - DigSumSingleRule,
\cr 5 - AddDoubleRule,
\cr 6 - MultDoubleRule,

}
  \item{range}{
a range from which constant values for the rules are generated.
Default range is from -15 to 15 (excluding 0).
For objects of class \code{DoubleRule} range is always set to NULL.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

createDictRule(4,range=c(7,2,15)) # creates a rule of multiplication by constant
#constant can be equal to 7,2, or 15





##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (index, range = c(-15:-1, 1:15)) 
{
    if (inherits(index, "SingleRule")) {
        rule <- index
        if ("constantVal" \%in\% slotNames(index)) {
            range = index@constantVal
        }
        else {
            range = NULL
        }
    }
    if (inherits(index, "DoubleRule")) {
        rule <- index
        range = NULL
    }
    if (class(index) == "numeric") {
        if (index > length(c(singleRules, doubleRules))) 
            stop("supplied index is larger than available list of rules")
        if (index <= length(singleRules)) {
            rule <- createSR(a1 = index, cv1 = 0, n = 0, previousRule = new("IdenSingleRule"))
        }
        else {
            rule <- createDR(a = (index - length(singleRules)), 
                fr = new("IdenSingleRule"), sr = new("IdenSingleRule"), 
                ns = new("IdenSingleRule"))
            range = NULL
        }
    }
    range = as.vector(range)
    b <- new("DictionaryRule", rule = rule, range = range)
    return(b)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
