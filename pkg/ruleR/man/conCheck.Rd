\name{conCheck}
\alias{conCheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function
}
\description{
A function that check whether generated number sequence is constant.
}
\usage{
conCheck(seq)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seq}{
numeric sequence as a list
}
}
\details{
\code{conCheck} is used by function \code{check}  
}
\value{

Function can return two values:

\item{1}{numeric sequence is not constant}
\item{0}{numeric sequence is constant}

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
conCheck(seq=list(1,1,1,1)) #0
conCheck(seq=list(1,2,3,4)) #1


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
