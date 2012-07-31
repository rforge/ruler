\name{AddRule}
\alias{AddRule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AddRule(rule)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rule}{
%%     ~~Describe \code{rule} here~~
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
function (rule) 
{
    if (!inherits(rule, "SingleRule") && !inherits(rule, "DoubleRule")) 
        stop(paste("new Rule must be an object of class 'SingleRule' od 'DoubleRule'. It cannot be of class", 
            class(rule)))
    s <- sequence(x1 = 10, x2 = 20, rule, n = 6)
    if (duplicate(VerifTable, unlist(s[[1]])) == TRUE) {
        stop("There already is a rule that gives the same result.")
    }
    else {
        VerifTable <<- rbind(unlist(s[[1]]), VerifTable)
        MyRules[length(MyRules) + 1] <<- rule
        print("Your rule succesfully added tp 'MyRule' list")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
