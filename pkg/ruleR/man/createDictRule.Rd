\name{createDictRule}
\alias{createDictRule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createDictRule
}
\description{
A function to create an object of class DictionaryRule
}
\usage{
createDictRule(index, range = c(-15:-1, 1:15))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{index}{A SingleRule or DoubleRule or one of the following indices:
\cr 1 - IdenSingleRule,
\cr 2 - AddConstSingleRule,
\cr 3 - MultConstSingleRule,
\cr 4 - DigSumSingleRule,
\cr 5 - AddDoubleRule,
\cr 6 - MultDoubleRule
}
  \item{range}{
The range from which constant values for the rules are generated.
Default range is from -15 to 15 (excluding 0). For objects of class \code{DoubleRule} range is always set to \code{NULL}.
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
createDictRule(4,range=c(7,2,15)) # creates a rule of multiplication by constant
# constant can be equal to 7, 2, or 15
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
