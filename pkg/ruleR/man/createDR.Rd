\name{createDR}
\alias{createDR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createDR(a = NULL, fr = NULL, sr = NULL, ns = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{fr}{
%%     ~~Describe \code{fr} here~~
}
  \item{sr}{
%%     ~~Describe \code{sr} here~~
}
  \item{ns}{
%%     ~~Describe \code{ns} here~~
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
function (a = NULL, fr = NULL, sr = NULL, ns = NULL) 
{
    if (!is.null(a) && a > length(doubleRules)) 
        stop(paste("The list of doublrRules is shoreter than ", 
            a, ".Please specify 'a' value, which is smaller than or equal to", 
            length(doubleRules)))
    if (!inherits(fr, "singleRule") && !is.null(fr)) 
        stop(paste("'fr' argument must inherit from class singleRule"))
    if (!inherits(sr, "singleRule") && !is.null(sr)) 
        stop(paste("'sr' argument must inherit from class singleRule"))
    if (!inherits(ns, "singleRule") && !is.null(ns)) 
        stop(paste("'ns' argument must inherit from class singleRule"))
    if (is.null(a)) 
        a <- sample(1:length(doubleRules), 1)
    a <- doubleRules[[a]]
    if (is.null(fr)) 
        fr <- sample(c(k = createSR(), k = new("IdenSingleRule")), 
            1, prob = c(0.5, 0.5))
    if (is.null(sr)) 
        sr <- sample(c(k = createSR(), k = new("IdenSingleRule")), 
            1, prob = c(0.3, 0.7))
    if (is.null(ns)) 
        ns <- sample(c(k = createSR(), k = new("IdenSingleRule")), 
            1, prob = c(0.3, 0.7))
    p <- new(a, firstRule = fr$k, secondRule = sr$k, nextSingle = ns$k)
    return(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
