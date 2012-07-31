\name{sequence}
\alias{sequence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sequence(x1, x2 = NULL, rule, n = 6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
%%     ~~Describe \code{x1} here~~
}
  \item{x2}{
%%     ~~Describe \code{x2} here~~
}
  \item{rule}{
%%     ~~Describe \code{rule} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
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
function (x1, x2 = NULL, rule, n = 6) 
{
    if (inherits(rule, "DoubleRule") && is.null(x2)) 
        stop(" If you want to use a DoubleRoule you need to specify x2")
    if (class(x1) != "numeric" || (class(x2) != "NULL" && class(x2) != 
        "numeric")) 
        stop("arguments 'x1', 'x2' must be of type 'numeric'.")
    if (!inherits(rule, "SingleRule") && !inherits(rule, "DoubleRule")) 
        stop("'rule' argument must inherit from 'SingleRule' or 'DoubleRule' class")
    if (n < 3) 
        stop("sequence must be longer than 3")
    k <- list()
    k[1] = x1
    if (inherits(rule, "SingleRule")) {
        for (i in 2:n) {
            k[i] <- calculate(x = rule, y = k[[i - 1]])
        }
    }
    else {
        k[2] = x2
        for (i in 3:n) {
            k[i] <- calculate(x = rule, y = k[[i - 2]], z = k[[i - 
                1]])
        }
    }
    return(list(k, rule))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
