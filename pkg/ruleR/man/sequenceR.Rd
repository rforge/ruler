\name{sequenceR}
\alias{sequenceR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function
}
\description{
A function to generate numeric sequence of specyfied lenght.
Is used in \code{automaticTest} but can be used separately.
Note that this function doesn't check for constant sequence or
too big/small elements in numeric sequence!
}
\usage{
sequenceR(start,rule,seqlen)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start}{
range from which starting values are generated
}
  \item{rule}{
rule that is to be used to generate numeric sequence
}
  \item{seqlen}{
the length of the numeric sequence (default value is 6)
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
function returns a list containing of:

\item{sequenceR[[1]] }{numeric sequence as a list}
\item{sequenceR[[2]]}{rule used to generate the sequence}

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
\code{\link{conCheck}},
\code{\link{check}},
\code{\link{createSR}},
\code{\link{createDR}}

}
\examples{
sequenceR(rule=new("AddConstSingleRule",constantVal=1,previousRule=new("IdenSingleRule")),start=99,seqlen=6)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
