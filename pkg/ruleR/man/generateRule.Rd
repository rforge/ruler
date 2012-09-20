\name{generateRule}
\alias{generateRule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
generateRule
}
\description{
A function to generate a rule with parameters given. Used in \code{val} function.
}
\usage{
generateRule(itemDictionary, chain, del)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{itemDictionary}{
%%     ~~Describe \code{itemDictionary} here~~
}
  \item{chain}{
%%     ~~Describe \code{chain} here~~
}
  \item{del}{
%%     ~~Describe \code{del} here~~
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
function (itemDictionary, chain, del) 
{
    if (class(itemDictionary) != "list") 
        stop("itemDictionary must be of class 'list'")
    h <- sample(1:length(itemDictionary), 1)
    a <- itemDictionary[[h]]@rule
    if (inherits(a, "SingleRule")) {
        if ("constantVal" \%in\% slotNames(a)) {
            cv <- generate_cv(itemDictionary[[h]])
            a@constantVal <- cv
        }
        itemDictionary <- itemDictionary[-h]
        if (length(itemDictionary) > 0) {
            dict_single <- split_dict(itemDictionary, 1)
            a <- nest(chain, dict_single, a)
        }
        else {
            dict_single <- NULL
            a <- a
        }
    }
    else {
        chain0 <- chain
        dict_single0 <- split_dict(itemDictionary, 1)
        if (1 \%in\% del) {
            chain <- chain0
            dict_single <- dict_single0
            h <- sample(1:length(dict_single), 1)
            b <- dict_single[[h]]@rule
            if ("constantVal" \%in\% slotNames(b)) 
                b@constantVal <- generate_cv(dict_single[[h]])
            dict_single <- dict_single[-h]
            chain = chain - 1
            if (chain >= 1) {
                a@firstRule <- nest(chain, dict_single, b)
            }
            else {
                a@firstRule <- b
            }
        }
        if (2 \%in\% del) {
            chain <- chain0
            dict_single <- dict_single0
            h <- sample(1:length(dict_single), 1)
            b <- dict_single[[h]]@rule
            if ("constantVal" \%in\% slotNames(b)) 
                b@constantVal <- generate_cv(dict_single[[h]])
            dict_single <- dict_single[-h]
            chain = chain - 1
            if (chain >= 1) {
                a@secondRule <- nest(chain, dict_single, b)
            }
            else {
                a@secondRule <- b
            }
        }
        if (3 \%in\% del) {
            chain <- chain0
            dict_single <- dict_single0
            h <- sample(1:length(dict_single), 1)
            b <- dict_single[[h]]@rule
            if ("constantVal" \%in\% slotNames(b)) 
                b@constantVal <- generate_cv(dict_single[[h]])
            dict_single <- dict_single[-h]
            chain = chain - 1
            if (chain >= 1) {
                a@nextSingle <- nest(chain, dict_single, b)
            }
            else {
                a@nextSingle <- b
            }
        }
    }
    return(a)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{internal}

