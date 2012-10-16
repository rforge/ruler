\name{val}
\alias{val}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Validate generated items
}
\description{
Save new items on a list if they are unique and not constant etc.
Not intended to be called directly.
}
\usage{
val(items, rules, itemDictionary, chain, del, start, element_range, seqlen, number)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{items}{
%%     ~~Describe \code{items} here~~
}
  \item{rules}{
%%     ~~Describe \code{rules} here~~
}
  \item{itemDictionary}{
%%     ~~Describe \code{itemDictionary} here~~
}
  \item{chain}{
%%     ~~Describe \code{chain} here~~
}
  \item{del}{
%%     ~~Describe \code{del} here~~
}
  \item{start}{
%%     ~~Describe \code{start} here~~
}
  \item{element_range}{
%%     ~~Describe \code{element_range} here~~
}
  \item{seqlen}{
%%     ~~Describe \code{seqlen} here~~
}
  \item{number}{
%%     ~~Describe \code{number} here~~
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
function (items, rules, itemDictionary, chain, del, start, element_range, 
    seqlen, number) 
{
    number = number
    rule <- generateRule(itemDictionary, chain, del)
    result <- sequence(start = start, rule = rule, seqlen = seqlen)[[1]]
    validation <- check(items, result, element_range)
    if (validation == 0) {
        if (number < 3) {
            number = number + 1
            val(items = items, rules = rules, itemDictionary = itemDictionary, 
                chain = chain, del = del, start = start, element_range = element_range, 
                seqlen = seqlen, number)
        }
        else {
            item <- rep(NA, seqlen)
            rule <- new("IdenSingleRule")
            k <- list(item, rule)
            return(k)
            break
        }
    }
    else {
        item <<- unlist(result)
        rule <- rule
        k <- list(item, rule)
        return(k)
        break
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{internal}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
