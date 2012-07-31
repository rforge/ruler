\name{automaticTest}
\alias{automaticTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
automaticTest(testlen, type = 1, seqlen = 6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{testlen}{
%%     ~~Describe \code{testlen} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{seqlen}{
%%     ~~Describe \code{seqlen} here~~
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
function (testlen, type = 1, seqlen = 6) 
{
    if (class(type) != "numeric") 
        stop("argument 'type' must be of class 'numeric'")
    items <- matrix(NA, testlen, seqlen)
    rules <- list()
    noise <- matrix(NA, testlen, 5)
    for (i in 1:testlen) {
        b <- check(seqlen, items, type)
        items[i, ] <- unlist(b[[1]])
        rules[i] <- b[2]
        noise[i, ] <- sample(c((items[i, seqlen] - 6):(items[i, 
            seqlen] + 6))[-(seqlen + 1)], 5)
    }
    return(list(items = items, noise = noise, rules = rules))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
