\name{nest}
\alias{nest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
nest
}
\description{
A function to nest several rules together.Used in \code{generateRule}. Not to be used by user.
}
\usage{
nest(chain, dict_single, a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chain}{
how many rules are to be nested
}
  \item{dict_single}{
a list of available singleRules
}
  \item{a}{
the rule to which other rules are to be nested
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
function (chain, dict_single, a) 
{
    if (chain > length(dict_single)) 
        chain <- length(dict_single)
    if (chain != 0) {
        p <- sample(1:length(dict_single), 1)
        k <- dict_single[[p]]@rule
        if ("constantVal" \%in\% slotNames(k)) 
            k@constantVal <- generate_cv(dict_single[[p]])
        a@previousRule <- k
        chain = chain - 1
        dict_single <- dict_single[-p]
        k <- nest(chain, dict_single, k)
        a@previousRule <- k
    }
    return(a)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{internal}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
