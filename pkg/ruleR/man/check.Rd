\name{check}
\alias{check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{function \code{check}
}
\description{function checking if generated sequence is constant, or has elements greater/smaller than the limit set by user.
}
\usage{check(items,result,element_range)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{items}{
a list of already generated items
}
  \item{result}{numeric sequence thatis to be checked

}
  \item{element_range}{
maximum and minimum values of elements of the numeric sequence
}
}
\details{
Function compares result with elements of list \code{items} that have the same length as 'result'argument
}
\value{
Result od \code{check} is a list:


\item{$result}{numeric sequence that is later added \code{items} matrix in \code{automaticTest}}
\item{$fun}{rule used to create the \code{$result} functioned that is later saved to \code{rules} list in \code{automaticTest}}

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
\code{\link{createTest}}

}
\examples{


}

\keyword{internal}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
