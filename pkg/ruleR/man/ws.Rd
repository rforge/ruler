\name{ws}
\alias{ws}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ws
}
\description{
helping function to print the name of rule
}
\usage{
ws(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
    if (inherits(x, "AddConstSingleRule")) {
        a1 = "Add"
        b1 = x@constantVal
    }
    if (inherits(x, "MultConstSingleRule")) {
        a1 = "Multiply the previous element of numeric sequence by"
        b1 = x@constantVal
    }
    if (inherits(x, "DigSumSingleRule")) {
        a1 = "Take sum of digits"
        b1 = NULL
    }
    if (inherits(x, "NegativeSingleRule")) {
        a1 = "Take a negative of the element"
        b1 = NULL
    }
    if (inherits(x, "IdenSingleRule")) {
        a1 = NULL
        b1 = NULL
    }
    return(list(a1, b1))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{internal}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
