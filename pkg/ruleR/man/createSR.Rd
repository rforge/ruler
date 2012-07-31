\name{createSR}
\alias{createSR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createSR(a1 = NULL, cv1 = NULL, n = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a1}{
%%     ~~Describe \code{a1} here~~
}
  \item{cv1}{
%%     ~~Describe \code{cv1} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (a1 = NULL, cv1 = NULL, n = NULL, ...) 
{
    p <- list(...)
    p <- unlist(p)
    if (is.null(a1)) {
        a1 <- sample(2:length(singleRules), 1)
    }
    if (is.null(cv1)) {
        cv1 <- sample(1:100, 1)
    }
    if (is.null(n)) {
        n <- sample(c(0, 1, 2), 1, prob = c(3/6, 2/6, 1/6))
        p <- as.vector(matrix(replicate(n, c(sample(1:length(singleRules), 
            1), sample(1:100, 1))), 1, 2 * n))
    }
    if ("constantVal" \%in\% slotNames(singleRules[[a1]])) {
        m <- new(singleRules[[a1]], constantVal = cv1)
    }
    else {
        m <- new(singleRules[[a1]])
    }
    if (n != 0) {
        createSR(p[[1]], p[[2]], n - 1, p[-c(1, 2)])
        m@previousRule <- k
    }
    else {
        return(m)
    }
    return(m)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
