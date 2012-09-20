\name{split_dict}
\alias{split_dict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
split_dict
}
\description{
function getting a list of SingleRules out of all available rules.
}
\usage{
split_dict(itemDictionary, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{itemDictionary}{
%%     ~~Describe \code{itemDictionary} here~~
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
function (itemDictionary, type) 
{
    lista <- list()
    if (type == 1) {
        b <- "SingleRule"
    }
    else {
        b <- "DoubleRule"
    }
    for (i in 1:length(itemDictionary)) {
        if (inherits(itemDictionary[[i]]@rule, b)) 
            lista <- c(lista, itemDictionary[[i]])
    }
    return(lista)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{internal}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
