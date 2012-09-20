\name{writing_double}
\alias{writing_double}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
writing_double
}
\description{
A function to print the rule (\code{DoubleRule})
}
\usage{
writing_double(x)
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
    if (inherits(x@firstRule, "IdenSingleRule")) {
        a1 = NULL
    }
    else {
        b <<- list()
        a1 = writing_single(x@firstRule)
    }
    if (inherits(x@secondRule, "IdenSingleRule")) {
        a2 = NULL
    }
    else {
        b <<- list()
        a2 = writing_single(x@secondRule)
    }
    if (inherits(x, "AddDoubleRule")) {
        a3 = "Take the sum of two previous elements of numeric sequence"
    }
    if (inherits(x, "MultDoubleRule")) {
        a3 = "Multiply two previous elements of numeric sequence"
    }
    if (inherits(x@nextSingle, "IdenSingleRule")) {
        a4 = NULL
    }
    else {
        b <<- list()
        a4 = writing_single(x@nextSingle)
    }
    return(list(a1, a2, a3, a4))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{internal}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
