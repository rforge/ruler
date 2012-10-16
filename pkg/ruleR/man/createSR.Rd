\name{createSR}
\alias{createSR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createSR
}
\description{
A function to create objects of class \code{SingleRule}
}
\usage{
createSR(a1 = NULL, cv1 = NULL, n = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a1}{
an index from table \code{SingleRule} (the default \code{NULL} leads to a randomly generated rule)
}
  \item{cv1}{
 a constant value
}
  \item{n}{
how many rules are to be nested? (the default \code{NULL} will randomly generate an \code{n} from \code{c(0,1,2)}
}
  \item{\dots}{
parameters for nested rules can be passed via this argument
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
createSR(a1=2,cv1=1,n=0)

createSR(a1=2,cv1=4,n=1,c(2,15))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
