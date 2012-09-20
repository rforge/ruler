\name{print.Rule}
\alias{print.Rule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function
}
\description{
a function defined to print objects of class \code{DoubleRule} or \code{SingleRule} in a common language.
}
\usage{
print.Rule(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class \code{DoubleRule} or \code{SingleRule}
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
\code{\link{createSR}},
\code{\link{createDR}},
\code{\linkS4class{SingleRule}},
\code{\linkS4class{DoubleRule}}

}
\examples{
m<-createTest(1)
print.Rule(m[[2]][[1]])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
