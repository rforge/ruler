\name{check}
\alias{check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
check(seqlen, items, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seqlen}{
%%     ~~Describe \code{seqlen} here~~
}
  \item{items}{
%%     ~~Describe \code{items} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (seqlen, items, type) 
{
    x1 <- as.numeric(sample(1:100, 1))
    x2 <- as.numeric(sample(1:100, 1))
    if (type == 1) {
        m <- sample(c(1, 2), 1)
        if (m == 1) {
            rule <- createSR()
        }
        else {
            rule <- createDR()
        }
    }
    else {
        z <- sample(2:length(MyRules), 1)
        rule <- MyRules[[z]]
    }
    result <- sequence(x1, x2, rule, n = seqlen)[[1]]
    fun <- sequence(x1, x2, rule, n = seqlen)[[2]]
    if (conCheck(result) == 0 || result[length(result)] > 1000 || 
        result[length(result)] < -1000 || duplicate(mx = items, 
        vec = unlist(result[[1]]))) {
        check(seqlen, items, type)
    }
    else {
        return(list(result, fun))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
